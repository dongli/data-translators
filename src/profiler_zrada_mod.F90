module profiler_zrada_mod

  use profiler_mod
  use datetime
  use string
  use hash_table_mod
  use linked_list_mod
  use params_mod
  use utils_mod

  implicit none

  private

  public profiler_zrada_read

contains

  subroutine profiler_zrada_read(file_paths, stations, records)

    character(*), intent(in) :: file_paths
    type(hash_table_type), intent(inout) :: stations
    type(linked_list_type), intent(inout) :: records

    character(8) station_name
    integer offset_s, offset_e
    character(255) file_path
    character(10) data_type
    character(2) device_type
    character(14) date_time_str
    character(100) line
    character(10) segment
    character(20) key
    real lon, lat, z, p, h, u, v, w, wd, ws
    integer wnd_h_qc, wnd_v_qc
    type(datetime_type) time
    type(profiler_station_type), pointer :: station
    type(profiler_record_type), pointer :: record
    type(linked_list_iterator_type) record_iterator

    stations = hash_table(chunk_size=50000, max_load_factor=0.9)
    call records%clear()
    nullify(record)

    offset_s = 1
    do while (.true.)
      offset_e = index(file_paths(offset_s:len_trim(file_paths)), ',')
      if (offset_e /= 0) then
        file_path = file_paths(offset_s:offset_e-1)
      else
        file_path = file_paths(offset_s:len_trim(file_paths))
      end if
      write(*, *) '[Notice]: Reading ' // trim(file_path) // ' ...'
      open(10, file=file_path)
      read(10, *)
      read(10, *) station_name, lon, lat, z, device_type, date_time_str
      time = create_datetime(date_time_str, '%Y%m%d%H%M%S')
      read(10, *) data_type
      if (data_type /= 'ROBS') then
        write(*, *) '[Warning]: Currently, only ROBS wind profiler are supported!'
        return
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
        read(line(1:5), *) h
        read(line(7:11), *) segment
        if (segment == '/////') then
          wd = real_missing_value
          ws = real_missing_value
        else
          read(line(7:11), *) wd
          read(line(13:17), *) ws
          read(line(26:28), *) wnd_h_qc
          u = wind_u_component(ws, wd)
          v = wind_v_component(ws, wd)
        end if
        read(line(19:24), *) segment
        if (segment == '//////') then
          w = real_missing_value
        else
          read(segment, *) w
          read(line(30:32), *) wnd_v_qc
        end if
        key = to_string(int(h))
        if (.not. is_missing(h))  call record%pro_hash%height%insert(key, h)
        if (.not. is_missing(u))  call record%pro_hash%wind_u%insert(key, u)
        if (.not. is_missing(v))  call record%pro_hash%wind_v%insert(key, v)
        if (.not. is_missing(wd)) call record%pro_hash%wind_direction%insert(key, wd)
        if (.not. is_missing(ws)) call record%pro_hash%wind_speed%insert(key, ws)
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
        call record%pro%init(record%pro_hash%height%size)
        call record%pro%set_from_hash(record%pro_hash)
        call record%station%records%insert(record)
        ! if (record%station%name == '54511') then
        !   call record%print()
        ! end if
      end select
      call record_iterator%next()
    end do

    write(*, *) '[Notice]: Station size is ' // trim(to_string(stations%size)) // ', record size is ' // trim(to_string(records%size)) // '.'

  end subroutine profiler_zrada_read

end module profiler_zrada_mod
