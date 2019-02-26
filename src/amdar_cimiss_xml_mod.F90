module amdar_cimiss_xml_mod

  use amdar_mod
  use datetime
  use hash_table_mod
  use linked_list_mod
  use regex
  use fox_sax
  use string_mod
  use params_mod
  use utils_mod

  implicit none

  private

  public amdar_cimiss_xml_read

  type(hash_table_type), pointer :: dummy_flights
  type(linked_list_type), pointer :: dummy_records

contains

  subroutine amdar_cimiss_xml_read(file_path, flights, records)

    character(*), intent(in) :: file_path
    type(hash_table_type), intent(inout), target :: flights
    type(linked_list_type), intent(inout), target :: records

    type(xml_t) xml
    integer i, iostat

    flights = hash_table(chunk_size=50000, max_load_factor=0.9)
    call records%clear()

    dummy_flights => flights
    dummy_records => records

    write(*, *) '[Notice]: Reading ' // trim(file_path) // ' ...'

    call open_xml_file(xml, file_path, iostat)

    call parse(xml, startElement_handler=startElement_handler)

    call stop_parser(xml)

    call close_xml_t(xml)

    write(*, *) '[Notice]: Flight size is ' // trim(to_string(flights%size)) // ', record size is ' // trim(to_string(records%size)) // '.'

  end subroutine amdar_cimiss_xml_read

  subroutine startElement_handler(uri, local_name, name, attributes)

    character(*), intent(in) :: uri
    character(*), intent(in) :: local_name
    character(*), intent(in) :: name
    type(dictionary_t), intent(in) :: attributes

    type(reg_matches), allocatable :: res(:)
    type(amdar_flight_type), pointer :: flight
    type(amdar_record_type), pointer :: record
    character(8) flight_name
    character(256) value
    real lat, lon, z1, z2
    integer year, month, day, hour, minute
    type(datetime_type) time
    real p, p_qc
    real T, T_qc
    real q, q_qc
    real Td, Td_qc
    real rh, rh_qc
    real wd, wd_qc
    real ws, ws_qc
    integer turb_idx
    integer i

    select case (name)
    case ('DS')
      do i = 1, getLength(attributes)
        select case (getQName(attributes, i))
        case ('requestParamstimes')
          res = regex_search(getValue(attributes, i), 'datacode=([^&]*)&')
          if (size(res) /= 1 .or. res(1)%match(2)%str /= 'UPAR_ARD_G_MUL_MUT_TAB') then
            write(*, *) '[Error]: Input file is not CIMISS UPAR_ARD_G_MUL_MUT_TAB!'
            stop 1
          end if 
        end select
      end do
    case ('R')
      do i = 1, getLength(attributes)
        value = getValue(attributes, i)
        select case (getQName(attributes, i))
        case ('Station_Id_C')
          flight_name = value
        case ('Lat')
          read(value, *) lat
        case ('Lon')
          read(value, *) lon
        case ('Flight_Heigh')
          read(value, *) z1
        case ('Heigh_Alti')
          read(value, *) z2
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
        case ('PRS_HWC')
          read(value, *) p
        case ('TEM')
          read(value, *) T
        case ('DPT')
          read(value, *) Td
        case ('RHU')
          read(value, *) rh
        case ('V13002')
          read(value, *) q
        case ('WIN_D')
          read(value, *) wd
        case ('WIN_S')
          read(value, *) ws
        case ('V11037')
          read(value, *) turb_idx
        end select
      end do
      time = create_datetime(year, month, day, hour, minute)
      p  = merge(real_missing_value, p,  is_missing(p,  src='cimiss'))
      T  = merge(real_missing_value, T,  is_missing(T,  src='cimiss'))
      Td = merge(real_missing_value, Td, is_missing(Td, src='cimiss'))
      q  = merge(real_missing_value, q,  is_missing(q,  src='cimiss'))
      rh = merge(real_missing_value, rh, is_missing(rh, src='cimiss'))
      ws = merge(real_missing_value, ws, is_missing(ws, src='cimiss'))
      wd = merge(real_missing_value, wd, is_missing(wd, src='cimiss'))
      turb_idx = merge(int_missing_value, turb_idx, is_missing(turb_idx, src='cimiss'))
      ! Create flight and record.
      if (dummy_flights%hashed(flight_name)) then
        select type (value => dummy_flights%value(flight_name))
        type is (amdar_flight_type)
          flight => value
        end select
      else
        allocate(flight)
        call flight%init(flight_name)
        call dummy_flights%insert(flight_name, flight)
      end if
      allocate(record)
      record%seq_id = dummy_records%size
      record%flight => flight
      record%time = time
      ! Set record.
      record%lon = merge(real_missing_value, lon, is_missing(lon, src='cimiss'))
      record%lat = merge(real_missing_value, lat, is_missing(lat, src='cimiss'))
      if (is_missing(z1, src='cimiss')) then
        record%amdar_height = z1
      else if (is_missing(z2, src='cimiss')) then
        record%amdar_height = z2
      else
        record%amdar_height = real_missing_value
      end if
      record%amdar_pressure = multiply(p, 100.0)
      record%amdar_temperature = T
      record%amdar_specific_humidity = q
      record%amdar_dewpoint = Td
      record%amdar_relative_humidity = rh
      record%amdar_wind_speed = ws
      record%amdar_wind_direction = wd
      record%amdar_wind_u = wind_u_component(ws, wd)
      record%amdar_wind_v = wind_v_component(ws, wd)
      record%amdar_turbulence_index = turb_idx
      call dummy_records%insert(flight_name // '@' // time%isoformat(), record)
      call flight%records%insert(trim(to_string(record%seq_id)), record, nodup=.true.)
    end select

  end subroutine startElement_handler

end module amdar_cimiss_xml_mod
