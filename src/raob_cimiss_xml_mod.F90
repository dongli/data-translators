module raob_cimiss_xml_mod

  use raob_mod
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

  public raob_cimiss_xml_read

  type(hash_table_type), pointer :: dummy_stations
  type(hash_table_type) dummy_records

contains

  subroutine raob_cimiss_xml_read(file_path, stations, records)

    character(*), intent(in) :: file_path
    type(hash_table_type), intent(inout), target :: stations
    type(linked_list_type), intent(inout), target :: records

    type(xml_t) xml
    integer i, iostat
    type(hash_table_iterator_type) record_iterator

    stations = hash_table(chunk_size=50000, max_load_factor=0.9)
    call records%clear()

    dummy_stations => stations
    dummy_records = hash_table(chunk_size=50000, max_load_factor=0.9)

    write(*, *) '[Notice]: Reading ' // trim(file_path) // ' ...'

    call open_xml_file(xml, file_path, iostat)

    call parse(xml, startElement_handler=startElement_handler)

    call stop_parser(xml)

    call close_xml_t(xml)

    ! Transfer read type to final type for easy use.
    record_iterator = hash_table_iterator(dummy_records)
    do while (.not. record_iterator%ended())
      select type (record => record_iterator%value)
      type is (raob_record_type)
        call record%snd_man %init(record%snd_man_hash %pressure%size)
        call record%snd_sigt%init(record%snd_sigt_hash %pressure%size)
        call record%snd_sigw%init(record%snd_sigw_hash %pressure%size)
        call record%snd_trop%init(record%snd_trop_hash%pressure%size)
        call record%snd_man %set_from_hash(record%snd_man_hash)
        call record%snd_sigt%set_from_hash(record%snd_sigt_hash)
        call record%snd_sigw%set_from_hash(record%snd_sigw_hash)
        call record%snd_trop%set_from_hash(record%snd_trop_hash)
        call record%station%records%insert(record)
        ! if (record%station%name == '54511') then
        !   call record%print()
        ! end if
        call records%insert(record%station%name // '@' // record%time%isoformat(), record)
      end select
      call record_iterator%next()
    end do

    write(*, *) '[Notice]: Flight size is ' // trim(to_string(stations%size)) // ', record size is ' // trim(to_string(records%size)) // '.'

  end subroutine raob_cimiss_xml_read

  subroutine startElement_handler(uri, local_name, name, attributes)

    character(*), intent(in) :: uri
    character(*), intent(in) :: local_name
    character(*), intent(in) :: name
    type(dictionary_t), intent(in) :: attributes

    type(reg_matches), allocatable :: res(:)
    type(raob_station_type), pointer :: station
    type(raob_record_type), pointer :: record
    character(8) station_name
    character(256) value
    character(20) record_key, level_key
    logical new_record
    real lat, lon, z
    integer year, month, day, hour, minute
    type(datetime_type) time
    real p, p_qc
    real h, h_qc, h1, h2
    real T, T_qc
    real q, q_qc
    real Td, Td_qc
    real wd, wd_qc
    real ws, ws_qc
    integer obs_type
    integer i

    select case (name)
    case ('DS')
      do i = 1, getLength(attributes)
        select case (getQName(attributes, i))
        case ('requestParamstimes')
          res = regex_search(getValue(attributes, i), 'datacode=([^&]*)&')
          if (size(res) /= 1 .or. res(1)%match(2)%str /= 'UPAR_CHN_MUL_FTM') then
            write(*, *) '[Error]: Input file is not CIMISS UPAR_CHN_MUL_FTM!'
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
        case ('EVSS')
          read(value, *) obs_type
        case ('PRS_HWC')
          read(value, *) p
        case ('Q_PRS_HWC')
          read(value, *) p_qc
        case ('GPH')
          read(value, *) h1
        case ('Q_GPH')
          read(value, *) h_qc
        case ('Heigh_Alti')
          read(value, *) h2
        case ('TEM')
          read(value, *) T
        case ('Q_TEM')
          read(value, *) T_qc
        case ('DPT')
          read(value, *) Td
        case ('Q_DPT')
          read(value, *) Td_qc
        case ('WIN_D')
          read(value, *) wd
        case ('Q_WIN_D')
          read(value, *) wd_qc
        case ('WIN_S')
          read(value, *) ws
        case ('Q_WIN_S')
          read(value, *) ws_qc
        end select
      end do
      time = create_datetime(year, month, day, hour, minute)
      h = merge(h1, h2, is_missing(h1, src='cimiss'))
      ! Create station and record.
      if (dummy_stations%hashed(station_name)) then
        select type (value => dummy_stations%value(station_name))
        type is (raob_station_type)
          station => value
        end select
      else
        allocate(station)
        call station%init(station_name, lon, lat, z)
        call dummy_stations%insert(station_name, station)
      end if
      record_key = trim(station_name) // '@' // time%isoformat() // '#' // to_string(obs_type)
      if (dummy_records%hashed(record_key)) then
        select type (value => dummy_records%value(record_key))
        type is (raob_record_type)
          record => value
        end select
      else
        allocate(record)
        call record%init(alloc_hash=.true.)
        record%seq_id = dummy_records%size
        record%station => station
        record%time = time
        new_record = .true.
      end if
      ! Set record.
      p = multiply(merge(real_missing_value, p, is_missing(p, src='cimiss')), 100.0)
      level_key = to_string(p)
      select case (obs_type)
      case (131072) ! Surface
        if (.not. is_missing(p))                record%snd_sfc_pressure = p
        if (.not. is_missing(T,  src='cimiss')) record%snd_sfc_temperature = T
        if (.not. is_missing(Td, src='cimiss')) record%snd_sfc_dewpoint = Td
        if (.not. is_missing(ws, src='cimiss')) record%snd_sfc_wind_speed = ws
        if (.not. is_missing(wd, src='cimiss')) record%snd_sfc_wind_direction = wd
      case (65536) ! Mandatory level
        if (.not. record%snd_man_hash%pressure%hashed(level_key) .and. .not. is_missing(p)) then
          call record%snd_man_hash%pressure%insert(level_key, p)
        end if
        if (.not. record%snd_man_hash%height%hashed(level_key) .and. .not. is_missing(h, src='cimiss')) then
          call record%snd_man_hash%height%insert(level_key, h)
        end if
        if (.not. record%snd_man_hash%temperature%hashed(level_key) .and. .not. is_missing(T, src='cimiss')) then
          call record%snd_man_hash%temperature%insert(level_key, T)
        end if
        if (.not. record%snd_man_hash%dewpoint%hashed(level_key) .and. .not. is_missing(Td, src='cimiss')) then
          call record%snd_man_hash%dewpoint%insert(level_key, Td)
        end if
        if (.not. record%snd_man_hash%wind_speed%hashed(level_key) .and. .not. is_missing(ws, src='cimiss')) then
          call record%snd_man_hash%wind_speed%insert(level_key, ws)
        end if
        if (.not. record%snd_man_hash%wind_direction%hashed(level_key) .and. .not. is_missing(wd, src='cimiss')) then
          call record%snd_man_hash%wind_direction%insert(level_key, wd)
        end if
      case (32768) ! Tropopause level
        if (.not. record%snd_trop_hash%pressure%hashed(level_key) .and. .not. is_missing(p)) then
          call record%snd_trop_hash%pressure%insert(level_key, p)
        end if
        if (.not. record%snd_trop_hash%temperature%hashed(level_key) .and. .not. is_missing(T, src='cimiss')) then
          call record%snd_trop_hash%temperature%insert(level_key, T)
        end if
        if (.not. record%snd_trop_hash%dewpoint%hashed(level_key) .and. .not. is_missing(Td, src='cimiss')) then
          call record%snd_trop_hash%dewpoint%insert(level_key, Td)
        end if
        if (.not. record%snd_trop_hash%wind_speed%hashed(level_key) .and. .not. is_missing(ws, src='cimiss')) then
          call record%snd_trop_hash%wind_speed%insert(level_key, ws)
        end if
        if (.not. record%snd_trop_hash%wind_direction%hashed(level_key) .and. .not. is_missing(wd, src='cimiss')) then
          call record%snd_trop_hash%wind_direction%insert(level_key, wd)
        end if
      case (8192)  ! Significant temperature level
        if (.not. record%snd_sigt_hash%pressure%hashed(level_key) .and. .not. is_missing(p)) then
          call record%snd_sigt_hash%pressure%insert(level_key, p)
        end if
        if (.not. record%snd_sigt_hash%temperature%hashed(level_key) .and. .not. is_missing(T, src='cimiss')) then
          call record%snd_sigt_hash%temperature%insert(level_key, T)
        end if
        if (.not. record%snd_sigt_hash%dewpoint%hashed(level_key) .and. .not. is_missing(Td, src='cimiss')) then
          call record%snd_sigt_hash%dewpoint%insert(level_key, Td)
        end if
      case (2048)  ! Significant wind level
        if (.not. record%snd_sigw_hash%pressure%hashed(level_key) .and. .not. is_missing(p)) then
          call record%snd_sigw_hash%pressure%insert(level_key, p)
        end if
        if (.not. record%snd_sigw_hash%height%hashed(level_key) .and. .not. is_missing(h, src='cimiss')) then
          call record%snd_sigw_hash%height%insert(level_key, h)
        end if
        if (.not. record%snd_sigw_hash%wind_speed%hashed(level_key) .and. .not. is_missing(ws, src='cimiss')) then
          call record%snd_sigw_hash%wind_speed%insert(level_key, ws)
        end if
        if (.not. record%snd_sigw_hash%wind_direction%hashed(level_key) .and. .not. is_missing(wd, src='cimiss')) then
          call record%snd_sigw_hash%wind_direction%insert(level_key, wd)
        end if
      case (16384, 4096) ! Unknown levels
        return
      case default
        write(*, *) '[Error]: Unknown raob level type!', obs_type
        stop 1
      end select
      if (new_record) then
        call dummy_records%insert(record_key, record)
      end if
    end select

  end subroutine startElement_handler

end module raob_cimiss_xml_mod
