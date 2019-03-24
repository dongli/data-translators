module ship_cimiss_xml_mod

  use ship_mod
  use datetime_mod
  use timedelta_mod
  use hash_table_mod
  use linked_list_mod
  use regex
  use fox_sax
  use string_mod
  use params_mod
  use utils_mod

  implicit none

  private

  public ship_cimiss_xml_read

  type(hash_table_type), pointer :: dummy_ships
  type(linked_list_type), pointer :: dummy_records

contains

  subroutine ship_cimiss_xml_read(file_path, ships, records)

    character(*), intent(in) :: file_path
    type(hash_table_type), intent(inout), target :: ships
    type(linked_list_type), intent(inout), target :: records

    type(xml_t) xml
    integer i, iostat

    ships = hash_table(chunk_size=50000, max_load_factor=0.9)
    call records%clear()

    dummy_ships => ships
    dummy_records => records

    write(*, *) '[Notice]: Reading ' // trim(file_path) // ' ...'

    call open_xml_file(xml, file_path, iostat)

    call parse(xml, startElement_handler=startElement_handler)

    call stop_parser(xml)

    call close_xml_t(xml)

    write(*, *) '[Notice]: Ship size is ' // trim(to_string(ships%size)) // ', record size is ' // trim(to_string(records%size)) // '.'

  end subroutine ship_cimiss_xml_read

  subroutine startElement_handler(uri, local_name, name, attributes)

    character(*), intent(in) :: uri
    character(*), intent(in) :: local_name
    character(*), intent(in) :: name
    type(dictionary_t), intent(in) :: attributes

    type(reg_matches), allocatable :: res(:)
    type(ship_type), pointer :: ship
    type(ship_record_type), pointer :: record
    character(10) ship_name
    character(256) value
    real lat, lon
    integer year, month, day, hour, minute
    type(datetime_type) time
    real wd, wd_qc
    real ws, ws_qc
    real wind_wave_height, wind_wave_height_qc
    real wind_wave_period, wind_wave_period_qc
    real surge_wave_height, surge_wave_height_qc
    real surge_wave_period, surge_wave_period_qc
    real vis, vis_qc
    real cld, cld_qc
    integer i

    select case (name)
    case ('DS')
      do i = 1, getLength(attributes)
        select case (getQName(attributes, i))
        case ('requestParamstimes')
          res = regex_search(getValue(attributes, i), 'datacode=([^&]*)&')
          if (size(res) /= 1 .or. res(1)%match(2)%str /= 'OCEN_GLB_SHB') then
            write(*, *) '[Error]: Input file is not CIMISS OCEN_GLB_SHB!'
            stop 1
          end if 
        end select
      end do
    case ('R')
      do i = 1, getLength(attributes)
        value = getValue(attributes, i)
        select case (getQName(attributes, i))
        case ('Station_Id_C')
          ship_name = value
        case ('Lat')
          read(value, *) lat
        case ('Lon')
          read(value, *) lon
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
        case ('WIN_D')
          read(value, *) wd
        case ('Q_WIN_D')
          read(value, *) wd_qc
        case ('WIN_S')
          read(value, *) ws
        case ('Q_WIN_S')
          read(value, *) ws_qc
        case ('WinWave_Heigh')
          read(value, *) wind_wave_height
        case ('Q_WinWave_Heigh')
          read(value, *) wind_wave_height_qc
        case ('WinWave_CYC')
          read(value, *) wind_wave_period
        case ('Q_WinWave_CYC')
          read(value, *) wind_wave_period_qc
        case ('SeaWave_1st_Heigh')
          read(value, *) surge_wave_height
        case ('Q_SeaWave_1st_Heigh')
          read(value, *) surge_wave_height_qc
        case ('SeaWave_1st_CYC')
          read(value, *) surge_wave_period
        case ('Q_SeaWave_1st_CYC')
          read(value, *) surge_wave_period_qc
        case ('VIS')
          read(value, *) vis
        case ('Q_VIS')
          read(value, *) vis_qc
        case ('CLO_Cov')
          read(value, *) cld
        case ('Q_CLO_Cov')
          read(value, *) cld_qc
        end select
      end do
      time = create_datetime(year, month, day, hour, minute)
      lon = merge(real_missing_value, lon, is_missing(lon, src='cimiss'))
      lat = merge(real_missing_value, lat, is_missing(lat, src='cimiss'))
      wd  = merge(real_missing_value, wd,  is_missing(wd,  src='cimiss'))
      ws  = merge(real_missing_value, ws,  is_missing(ws,  src='cimiss'))
      vis = merge(real_missing_value, vis, is_missing(vis, src='cimiss'))
      cld = merge(real_missing_value, cld, is_missing(cld, src='cimiss'))
      wind_wave_height  = merge(real_missing_value, wind_wave_height,  is_missing(wind_wave_height,  src='cimiss'))
      wind_wave_period  = merge(real_missing_value, wind_wave_period,  is_missing(wind_wave_period,  src='cimiss'))
      surge_wave_height = merge(real_missing_value, surge_wave_height, is_missing(surge_wave_height, src='cimiss'))
      surge_wave_period = merge(real_missing_value, surge_wave_period, is_missing(surge_wave_period, src='cimiss'))
      ! Create ship and record.
      if (dummy_ships%hashed(ship_name)) then
        select type (value => dummy_ships%value(ship_name))
        type is (ship_type)
          ship => value
        end select
      else
        allocate(ship)
        call ship%init(ship_name)
        ship%seq_id = dummy_ships%size
        call dummy_ships%insert(ship_name, ship)
      end if
      allocate(record)
      record%seq_id = dummy_records%size
      record%ship => ship
      record%time = time
      ! Set record.
      record%lon = lon
      record%lat = lat
      record%ship_wind_direction = wd
      record%ship_wind_speed = ws
      record%ship_wind_u = wind_u_component(ws, wd)
      record%ship_wind_v = wind_v_component(ws, wd)
      record%ship_wind_wave_height = wind_wave_height
      record%ship_wind_wave_period = wind_wave_period
      record%ship_surge_wave_height = surge_wave_height
      record%ship_surge_wave_period = surge_wave_period
      record%ship_visibility = vis
      record%ship_cloud_cover = cld
      ! TODO: How to map CIMISS QC to PrepBUFR QC?
      record%ship_wind_direction_qc = wd_qc
      record%ship_wind_speed_qc = ws_qc
      record%ship_wind_wave_height_qc = wind_wave_height_qc
      record%ship_wind_wave_period_qc = wind_wave_period_qc
      record%ship_surge_wave_height_qc = surge_wave_height_qc
      record%ship_surge_wave_period_qc = surge_wave_period_qc
      record%ship_visibility_qc = vis_qc
      record%ship_cloud_cover_qc = cld_qc
      call dummy_records%insert(ship_name // '@' // time%isoformat(), record)
      call ship%records%insert(trim(to_string(record%seq_id)), record, nodup=.true.)
    end select

  end subroutine startElement_handler

end module ship_cimiss_xml_mod
