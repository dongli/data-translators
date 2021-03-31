module profiler_zrada_mod

  use datetime
  use string
  use container
  use flogger
  use params_mod
  use data_translators_utils_mod
  use profiler_mod

  implicit none

  private

  public profiler_zrada_read

contains

  subroutine profiler_zrada_read(file_paths, stations, records)

    character(*), intent(in) :: file_paths
    type(hash_table_type), intent(inout) :: stations
    type(linked_list_type), intent(inout) :: records

    character(8) station_name
    integer iostat, offset_s, offset_e
    character(255) file_path
    character(10) data_type
    character(2) device_type
    character(14) date_time_str
    character(100) line
    character(20) key
    integer h
    real lon, lat, z, p, ua, va, wa, wd, ws
    integer wnd_h_qc, wnd_v_qc
    type(datetime_type) time
    type(profiler_station_type), pointer :: station
    type(profiler_record_type), pointer :: record
    type(linked_list_iterator_type) record_iterator

    offset_s = 1
    do while (.true.)
      offset_e = index(file_paths(offset_s:len_trim(file_paths)), ',')
      if (offset_e /= 0) then
        file_path = file_paths(offset_s:offset_e-1)
      else
        file_path = file_paths(offset_s:len_trim(file_paths))
      end if
      call log_notice('Reading ' // trim(file_path) // ' ...')
      open(10, file=file_path)
      read(10, '(A)', iostat=iostat)
      if (iostat /= 0) then
        call log_warning('File ' // trim(file_path) // ' is empty!')
        if (offset_e == 0) exit
        offset_s = offset_e + 1
        cycle
      end if
      read(10, *) station_name, lon, lat, z, device_type, date_time_str
      time = create_datetime(date_time_str, '%Y%m%d%H%M%S')
      read(10, *) data_type
      if (data_type /= 'ROBS') then
        call log_warning('Currently, only ROBS wind profiler are supported!')
        cycle
      end if
      if (stations%hashed(station_name)) then
        select type (value => stations%value(station_name))
        type is (profiler_station_type)
          station => value
        end select
      else
        allocate(station)
        call station%init(station_name, lon, lat, z)
        call stations%insert(station_name, station)
      end if
      allocate(record)
      call record%init(alloc_hash=.true.)
      record%seq_id = records%size
      record%station => station
      record%time = time
      do while (.true.)
        read(10, '(A)') line
        if (line == 'NNNN') exit
        read(line(1:5), '(I5)') h
        if (line(7:11) == '/////') then
          wd = real_missing_value
          ws = real_missing_value
        else
          read(line( 7:11), '(F5.1)', iostat=iostat) wd
          if (iostat /= 0) wd = real_missing_value
          read(line(13:17), '(F5.1)', iostat=iostat) ws
          if (iostat /= 0) ws = real_missing_value
          read(line(26:28), '(I3)', iostat=iostat) wnd_h_qc
          if (iostat /= 0) wnd_h_qc = int_missing_value
          ua = wind_u_component(ws, wd)
          va = wind_v_component(ws, wd)
        end if
        if (line(19:24) == '//////') then
          wa = real_missing_value
        else
          read(line(19:24), '(F5.1)', iostat=iostat) wa
          if (iostat /= 0) wa = real_missing_value
          read(line(30:32), '(I3)', iostat=iostat) wnd_v_qc
          if (iostat /= 0) wnd_v_qc = int_missing_value
        end if
        key = to_str(h)
        if (.not. is_missing(h )) call record%pro_hash%h %insert(key, real(h))
        if (.not. is_missing(ua)) call record%pro_hash%ua%insert(key, ua)
        if (.not. is_missing(va)) call record%pro_hash%va%insert(key, va)
        if (.not. is_missing(wd)) call record%pro_hash%wd%insert(key, wd)
        if (.not. is_missing(ws)) call record%pro_hash%ws%insert(key, ws)
      end do
      close(10)

      call records%insert(station_name // '@' // time%isoformat(), record)

      if (offset_e == 0) exit
      offset_s = offset_e + 1
    end do

    ! Transfer read type to final type for easy use.
    record_iterator = linked_list_iterator(records)
    do while (.not. record_iterator%ended())
      select type (record => record_iterator%value)
      type is (profiler_record_type)
        if (associated(record%pro_hash)) then
          call record%pro%init(record%pro_hash%h%size)
          call record%pro%set_from_hash(record%pro_hash)
          call record%station%records%insert(record)
        end if
      end select
      call record_iterator%next()
    end do

    call log_notice('Station size is ' // trim(to_str(stations%size)) // ', record size is ' // trim(to_str(records%size)) // '.')

  end subroutine profiler_zrada_read

end module profiler_zrada_mod
