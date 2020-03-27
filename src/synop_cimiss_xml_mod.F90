module synop_cimiss_xml_mod

  use datetime
  use string
  use container
  use flogger
  use regex
  use fox_sax
  use params_mod
  use utils_mod
  use synop_mod

  implicit none

  private

  public synop_cimiss_xml_read

  type(hash_table_type), pointer :: dummy_stations
  type(linked_list_type), pointer :: dummy_records

contains

  subroutine synop_cimiss_xml_read(file_path, stations, records)

    character(*), intent(in) :: file_path
    type(hash_table_type), intent(inout), target :: stations
    type(linked_list_type), intent(inout), target :: records

    type(xml_t) xml
    integer i, iostat

    dummy_stations => stations
    dummy_records => records

    call log_notice('Reading ' // trim(file_path) // ' ...')

    call open_xml_file(xml, file_path, iostat)

    call parse(xml, startElement_handler=startElement_handler)

    call stop_parser(xml)

    call close_xml_t(xml)

    call log_notice('Station size is ' // trim(to_string(stations%size)) // ', record size is ' // trim(to_string(records%size)) // '.')

  end subroutine synop_cimiss_xml_read

  subroutine startElement_handler(uri, local_name, name, attributes)

    character(*), intent(in) :: uri
    character(*), intent(in) :: local_name
    character(*), intent(in) :: name
    type(dictionary_t), intent(in) :: attributes

    type(reg_matches), allocatable :: res(:)
    type(synop_station_type), pointer :: station
    type(synop_record_type), pointer :: record
    character(10) station_name
    character(256) value
    real lat, lon, z
    integer year, month, day, hour, minute
    type(datetime_type) time
    real p, p_qc
    real ta, ta_qc
    real td, sh
    real rh, rh_qc
    real wd, wd_qc
    real ws, ws_qc
    integer i

    select case (name)
    case ('DS')
      do i = 1, getLength(attributes)
        select case (getQName(attributes, i))
        case ('requestParams')
          res = regex_search(getValue(attributes, i), 'datacode=([^&]*)&')
          if (size(res) /= 1 .or. res(1)%match(2)%str /= 'SURF_CHN_MAIN_MIN') then
            write(*, *) '[Error]: Input file is not CIMISS SURF_CHN_MAIN_MIN!'
            stop 1
          end if 
        end select
      end do
    case ('R')
      do i = 1, getLength(attributes)
        value = getValue(attributes, i)
        select case (getQName(attributes, i))
        case ('Station_Id_C')
          station_name = value
        case ('Lat')
          read(value, *) lat
        case ('Lon')
          read(value, *) lon
        case ('Alti')
          read(value, *) z
        case ('Year')
          read(value, *) year
        case ('Mon')
          read(value, *) month
        case ('Day')
          read(value, *) day
        case ('Hour')
          read(value, *) hour
        case ('Min')
          read(value, *) minute
        case ('PRS')
          read(value, *) p
        case ('TEM')
          read(value, *) ta
        case ('RHU')
          read(value, *) rh
        case ('WIN_D_Avg_1mi')
          read(value, *) wd
        case ('WIN_S_Avg_1mi')
          read(value, *) ws
        case ('Q_PRS')
          read(value, *) p_qc
        case ('Q_TEM')
          read(value, *) ta_qc
        case ('Q_RHU')
          read(value, *) rh_qc
        case ('Q_WIN_D_Avg_1mi')
          read(value, *) wd_qc
        case ('Q_WIN_S_Avg_1mi')
          read(value, *) ws_qc
        end select
      end do
      time = create_datetime(year, month, day, hour, minute)
      p  = merge(real_missing_value, p,  is_missing(p,  src='cimiss'))
      ta = merge(real_missing_value, ta, is_missing(ta, src='cimiss'))
      rh = merge(real_missing_value, rh, is_missing(rh, src='cimiss'))
      sh = specific_humidity_from_relative_humidity(p, ta, rh)
      td = dewpoint(p, sh)
      wd = merge(real_missing_value, wd, is_missing(wd, src='cimiss'))
      ws = merge(real_missing_value, ws, is_missing(ws, src='cimiss'))
      ! Create station and record.
      if (dummy_stations%hashed(station_name)) then
        select type (value => dummy_stations%value(station_name))
        type is (synop_station_type)
          station => value
        end select
      else
        allocate(station)
        call station%init(station_name, lon, lat, z)
        call dummy_stations%insert(station_name, station)
      end if
      allocate(record)
      record%seq_id = dummy_records%size
      record%station => station
      record%time = time
      ! Set record.
      record%p     = p
      record%ta    = td
      record%td    = td
      record%rh    = rh
      record%sh    = sh
      record%wd    = wd
      record%ws    = ws
      record%ua    = wind_u_component(ws, wd)
      record%va    = wind_v_component(ws, wd)
      record%p_qc  = merge(2, 3, p_qc  == 0)
      record%ta_qc = merge(2, 3, ta_qc == 0)
      record%rh_qc = merge(2, 3, rh_qc == 0)
      record%sh_qc = merge(2, 3, rh_qc == 0)
      record%wd_qc = merge(2, 3, wd_qc == 0)
      record%wd_qc = merge(2, 3, ws_qc == 0)
      ! if (station_name == 'V8552') then
      !   print *, record%print()
      ! end if
      call dummy_records%insert(station_name // '@' // time%isoformat(), record)
      call station%records%insert(trim(to_string(record%seq_id)), record, nodup=.true.)
    end select

  end subroutine startElement_handler

end module synop_cimiss_xml_mod
