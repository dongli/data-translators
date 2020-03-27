module raob_cimiss_xml_mod

  use raob_mod
  use datetime
  use string
  use container
  use flogger
  use regex
  use fox_sax
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

    dummy_stations => stations
    dummy_records = hash_table(chunk_size=50000, max_load_factor=0.9)

    call log_notice('Reading ' // trim(file_path) // ' ...')

    call open_xml_file(xml, file_path, iostat)

    call parse(xml, startElement_handler=startElement_handler)

    call stop_parser(xml)

    call close_xml_t(xml)

    ! Transfer read type to final type for easy use.
    record_iterator = hash_table_iterator(dummy_records)
    do while (.not. record_iterator%ended())
      select type (record => record_iterator%value)
      type is (raob_record_type)
        call record%man %init(record%man_hash %p%size)
        call record%sigt%init(record%sigt_hash%p%size)
        call record%sigw%init(record%sigw_hash%p%size)
        call record%trop%init(record%trop_hash%p%size)
        if (associated(record%man_hash)) then
          call record%man %set_from_hash(record%man_hash)
          call record%sigt%set_from_hash(record%sigt_hash)
          call record%sigw%set_from_hash(record%sigw_hash)
          call record%trop%set_from_hash(record%trop_hash)
          call record%station%records%insert(record)
          call records%insert(record%station%name // '@' // record%time%isoformat(), record)
        end if
      end select
      call record_iterator%next()
    end do

    call log_notice('Station size is ' // trim(to_string(stations%size)) // ', record size is ' // trim(to_string(records%size)) // '.')

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
    real ta, ta_qc
    real td, td_qc
    real wd, wd_qc
    real ws, ws_qc
    real rh, sh, ua, va
    integer obs_type
    integer i

    select case (name)
    case ('DS')
      do i = 1, getLength(attributes)
        select case (getQName(attributes, i))
        case ('requestParams')
          res = regex_search(getValue(attributes, i), 'datacode=([^&]*)&')
          if (size(res) /= 1 .or. res(1)%match(2)%str /= 'UPAR_CHN_MUL_FTM') then
            call log_error('Input file is not CIMISS UPAR_CHN_MUL_FTM!')
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
          read(value, *) ta
        case ('Q_TEM')
          read(value, *) ta_qc
        case ('DPT')
          read(value, *) td
        case ('Q_DPT')
          read(value, *) td_qc
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
      h  = merge(h1, h2, is_missing(h1, src='cimiss'))
      p  = multiply(merge(real_missing_value, p, is_missing(p, src='cimiss')), 100.0)
      ta = merge(real_missing_value, ta, is_missing(ta, src='cimiss'))
      td = merge(real_missing_value, td, is_missing(td, src='cimiss'))
      sh = specific_humidity_from_dewpoint(p, ta, td)
      rh = relative_humidity(p, ta, sh)
      ws = merge(real_missing_value, ws, is_missing(ws, src='cimiss'))
      wd = merge(real_missing_value, wd, is_missing(wd, src='cimiss'))
      ua = wind_u_component(ws, wd)
      va = wind_v_component(ws, wd)
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
      level_key = to_string(int(p))
      select case (obs_type)
      case (131072) ! Surface
        record%ps  = p
        record%tas = ta
        record%tds = td
        record%wss = ws
        record%wds = wd
        record%uas = ua
        record%vas = va
      case (65536) ! Mandatory level
        if (.not. record%man_hash%p%hashed(level_key) .and. .not. is_missing(p)) then
          call record%man_hash%p%insert(level_key, p)
        end if
        if (.not. record%man_hash%h%hashed(level_key) .and. .not. is_missing(h)) then
          call record%man_hash%h%insert(level_key, h)
        end if
        if (.not. record%man_hash%ta%hashed(level_key) .and. .not. is_missing(ta)) then
          call record%man_hash%ta%insert(level_key, ta)
        end if
        if (.not. record%man_hash%td%hashed(level_key) .and. .not. is_missing(td)) then
          call record%man_hash%td%insert(level_key, td)
        end if
        if (.not. record%man_hash%sh%hashed(level_key) .and. .not. is_missing(sh)) then
          call record%man_hash%sh%insert(level_key, sh)
        end if
        if (.not. record%man_hash%rh%hashed(level_key) .and. .not. is_missing(rh)) then
          call record%man_hash%rh%insert(level_key, rh)
        end if
        if (.not. record%man_hash%ws%hashed(level_key) .and. .not. is_missing(ws)) then
          call record%man_hash%ws%insert(level_key, ws)
        end if
        if (.not. record%man_hash%wd%hashed(level_key) .and. .not. is_missing(wd)) then
          call record%man_hash%wd%insert(level_key, wd)
        end if
        if (.not. record%man_hash%ua%hashed(level_key) .and. .not. is_missing(ua)) then
          call record%man_hash%ua%insert(level_key, ua)
        end if
        if (.not. record%man_hash%va%hashed(level_key) .and. .not. is_missing(va)) then
          call record%man_hash%va%insert(level_key, va)
        end if
      case (32768) ! Tropopause level
        if (.not. record%trop_hash%p%hashed(level_key) .and. .not. is_missing(p)) then
          call record%trop_hash%p%insert(level_key, p)
        end if
        if (.not. record%trop_hash%ta%hashed(level_key) .and. .not. is_missing(ta, src='cimiss')) then
          call record%trop_hash%ta%insert(level_key, ta)
        end if
        if (.not. record%trop_hash%td%hashed(level_key) .and. .not. is_missing(td, src='cimiss')) then
          call record%trop_hash%td%insert(level_key, td)
        end if
        if (.not. record%trop_hash%sh%hashed(level_key) .and. .not. is_missing(sh)) then
          call record%trop_hash%sh%insert(level_key, sh)
        end if
        if (.not. record%trop_hash%rh%hashed(level_key) .and. .not. is_missing(rh)) then
          call record%trop_hash%rh%insert(level_key, rh)
        end if
        if (.not. record%trop_hash%ws%hashed(level_key) .and. .not. is_missing(ws, src='cimiss')) then
          call record%trop_hash%ws%insert(level_key, ws)
        end if
        if (.not. record%trop_hash%wd%hashed(level_key) .and. .not. is_missing(wd, src='cimiss')) then
          call record%trop_hash%wd%insert(level_key, wd)
        end if
        if (.not. record%trop_hash%ua%hashed(level_key) .and. .not. is_missing(ua)) then
          call record%trop_hash%ua%insert(level_key, ua)
        end if
        if (.not. record%trop_hash%va%hashed(level_key) .and. .not. is_missing(va)) then
          call record%trop_hash%va%insert(level_key, va)
        end if
      case (8192)  ! Significant temperature level
        if (.not. record%sigt_hash%p%hashed(level_key) .and. .not. is_missing(p)) then
          call record%sigt_hash%p%insert(level_key, p)
        end if
        if (.not. record%sigt_hash%ta%hashed(level_key) .and. .not. is_missing(ta, src='cimiss')) then
          call record%sigt_hash%ta%insert(level_key, ta)
        end if
        if (.not. record%sigt_hash%td%hashed(level_key) .and. .not. is_missing(td, src='cimiss')) then
          call record%sigt_hash%td%insert(level_key, td)
        end if
        if (.not. record%sigt_hash%sh%hashed(level_key) .and. .not. is_missing(sh)) then
          call record%sigt_hash%sh%insert(level_key, sh)
        end if
        if (.not. record%sigt_hash%rh%hashed(level_key) .and. .not. is_missing(rh)) then
          call record%sigt_hash%rh%insert(level_key, rh)
        end if
      case (2048)  ! Significant wind level
        if (.not. record%sigw_hash%p%hashed(level_key) .and. .not. is_missing(p)) then
          call record%sigw_hash%p%insert(level_key, p)
        end if
        if (.not. record%sigw_hash%h%hashed(level_key) .and. .not. is_missing(h, src='cimiss')) then
          call record%sigw_hash%h%insert(level_key, h)
        end if
        if (.not. record%sigw_hash%ws%hashed(level_key) .and. .not. is_missing(ws, src='cimiss')) then
          call record%sigw_hash%ws%insert(level_key, ws)
        end if
        if (.not. record%sigw_hash%wd%hashed(level_key) .and. .not. is_missing(wd, src='cimiss')) then
          call record%sigw_hash%wd%insert(level_key, wd)
        end if
        if (.not. record%sigw_hash%ua%hashed(level_key) .and. .not. is_missing(ua)) then
          call record%sigw_hash%ua%insert(level_key, ua)
        end if
        if (.not. record%sigw_hash%va%hashed(level_key) .and. .not. is_missing(va)) then
          call record%sigw_hash%va%insert(level_key, va)
        end if
      case (16384, 4096) ! Unknown levels
        return
      case default
        call log_error('Unknown raob level type!')
      end select
      if (new_record) then
        call dummy_records%insert(record_key, record)
      end if
    end select

  end subroutine startElement_handler

end module raob_cimiss_xml_mod
