module synop_ftm_txt_mod

  use datetime
  use string
  use container
  use flogger
  use cli_mod
  use data_translators_utils_mod
  use synop_mod

  implicit none

  private

  public synop_ftm_txt_read

contains

  subroutine synop_ftm_txt_read(file_path_list, stations, records)

    character(*), intent(in) :: file_path_list
    type(hash_table_type), intent(inout) :: stations
    type(linked_list_type), intent(inout) :: records

    type(string_type), allocatable :: file_paths(:)
    type(datetime_type) time
    type(synop_station_type), pointer :: station
    type(synop_record_type), pointer :: record
    type(hash_table_type) dummy_records
    type(hash_table_iterator_type) dummy_record_iterator
    character(5) station_name
    real lon, lat, z
    real t02, t08, t14, t20
    integer wd02, wd08, wd14, wd20
    real ws02, ws08, ws14, ws20
    integer t02_qc, t08_qc, t14_qc, t20_qc
    integer wd02_qc, wd08_qc, wd14_qc, wd20_qc
    integer ws02_qc, ws08_qc, ws14_qc, ws20_qc
    integer k, year, month, day, iostat
    logical new_record

    dummy_records = hash_table(chunk_size=500000, max_load_factor=0.9)

    file_paths = split_string(file_path_list, ',')

    do k = 1, size(file_paths)
      call log_notice('Reading ' // trim(file_paths(k)%value) // ' ...')
      open(10, file=file_paths(k)%value, status='old')
      if (count_string(file_paths(k)%value, 'TEM') > 0) then
        do while (.true.)
          read(10, *, iostat=iostat) station_name, lat, lon, z, year, month, day, &
                                     t02, t08, t14, t20, t02_qc, t08_qc, t14_qc, t20_qc
          if (iostat < 0) then
            exit
          else if (iostat > 0) then
            call log_error('Failed to read ' // trim(file_paths(k)%value) // '!')
          end if

          station => create_station(stations, station_name, lon, lat, z)

          time = create_datetime(year, month, day, 2)
          record => create_record(dummy_records, station, time, new_record)
          record%ta = t02 / 10.0
          record%ta_qc = t02_qc
          if (new_record) call dummy_records%insert(trim(station%name) // ' ' // time%isoformat(), record)

          time = create_datetime(year, month, day, 8)
          record => create_record(dummy_records, station, time, new_record)
          record%ta = t08 / 10.0
          record%ta_qc = t08_qc
          if (new_record) call dummy_records%insert(trim(station%name) // ' ' // time%isoformat(), record)

          time = create_datetime(year, month, day, 14)
          record => create_record(dummy_records, station, time, new_record)
          record%ta = t14 / 10.0
          record%ta_qc = t14_qc
          if (new_record) call dummy_records%insert(trim(station%name) // ' ' // time%isoformat(), record)

          time = create_datetime(year, month, day, 20)
          record => create_record(dummy_records, station, time, new_record)
          record%ta = t20 / 10.0
          record%ta_qc = t20_qc
          if (new_record) call dummy_records%insert(trim(station%name) // ' ' // time%isoformat(), record)
        end do
      else if (count_string(file_paths(k)%value, 'WIN') > 0) then
        do while (.true.)
          read(10, *, iostat=iostat) station_name, lat, lon, z, year, month, day, &
                                     wd02, ws02, wd08, ws08, wd14, ws14, wd20, ws20, &
                                     wd02_qc, ws02_qc, wd08_qc, ws08_qc, wd14_qc, ws14_qc, wd20_qc, ws20_qc
          if (iostat < 0) then
            exit
          else if (iostat > 0) then
            call log_error('Failed to read ' // trim(file_paths(k)%value) // '!')
          end if

          station => create_station(stations, station_name, lon, lat, z)

          time = create_datetime(year, month, day, 2)
          record => create_record(dummy_records, station, time, new_record)
          record%wd = decode_wind_direction(wd02)
          record%wd_qc = wd02_qc
          record%ws = ws02
          record%ws_qc = ws02_qc
          if (new_record) call dummy_records%insert(trim(station%name) // ' ' // time%isoformat(), record)

          time = create_datetime(year, month, day, 8)
          record => create_record(dummy_records, station, time, new_record)
          record%wd = decode_wind_direction(wd08)
          record%wd_qc = wd08_qc
          record%ws = ws08
          record%ws_qc = ws08_qc
          if (new_record) call dummy_records%insert(trim(station%name) // ' ' // time%isoformat(), record)

          time = create_datetime(year, month, day, 14)
          record => create_record(dummy_records, station, time, new_record)
          record%wd = decode_wind_direction(wd14)
          record%wd_qc = wd14_qc
          record%ws = ws14
          record%ws_qc = ws14_qc
          if (new_record) call dummy_records%insert(trim(station%name) // ' ' // time%isoformat(), record)

          time = create_datetime(year, month, day, 20)
          record => create_record(dummy_records, station, time, new_record)
          record%wd = decode_wind_direction(wd20)
          record%wd_qc = wd20_qc
          record%ws = ws20
          record%ws_qc = ws20_qc
          if (new_record) call dummy_records%insert(trim(station%name) // ' ' // time%isoformat(), record)
        end do
      end if
      close(10)
    end do

    dummy_record_iterator = hash_table_iterator(dummy_records)
    do while (.not. dummy_record_iterator%ended())
      select type (record => dummy_record_iterator%value)
      type is (synop_record_type)
        if (cli_start_time%year == 0 .or. (cli_start_time <= record%time .and. record%time <= cli_end_time)) then
          call records%insert(record)
          if (record%station%name == cli_verbose_platform) then
            call record%print()
          end if
        end if
      end select
      call dummy_record_iterator%next()
    end do

    call log_notice('Station size is ' // trim(to_str(stations%size)) // ', record size is ' // trim(to_str(records%size)) // '.')

  end subroutine synop_ftm_txt_read

  function create_station(stations, station_name, lon, lat, z) result(station)

    type(hash_table_type), intent(inout) :: stations
    character(*), intent(in) :: station_name
    real, intent(inout) :: lon
    real, intent(inout) :: lat
    real, intent(in) :: z
    type(synop_station_type), pointer :: station

    lat = lat / 100 + mod(lat, 100.0) / 60
    lon = lon / 100 + mod(lon, 100.0) / 60

    if (stations%hashed(station_name)) then
      select type (value => stations%value(station_name))
      type is (synop_station_type)
        station => value
      end select
    else
      allocate(station)
      call station%init(station_name, lon, lat, z)
      call stations%insert(station_name, station)
    end if

  end function create_station

  function create_record(records, station, time, new_record) result(record)

    type(hash_table_type), intent(inout) :: records
    type(synop_station_type), intent(inout), pointer :: station
    type(datetime_type), intent(in) :: time
    logical, intent(out) :: new_record
    type(synop_record_type), pointer :: record

    if (records%hashed(trim(station%name) // ' ' // time%isoformat())) then
      select type (value => records%value(trim(station%name) // ' ' // time%isoformat()))
      type is (synop_record_type)
        record => value
      end select
      new_record = .false.
    else
      allocate(record)
      record%seq_id = records%size
      record%station => station
      record%time = time
      new_record = .true.
    end if

  end function create_record

end module synop_ftm_txt_mod
