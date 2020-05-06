module anem_txt_mod

  use anem_nrg_mod
  use datetime
  use string
  use container
  use flogger
  use regex
  use params_mod
  use utils_mod
  use cli_mod

  implicit none

  private

  public anem_txt_read

contains

  subroutine anem_txt_read(file_path, towers, records)

    character(*), intent(in) :: file_path
    type(hash_table_type), intent(inout), target :: towers
    type(linked_list_type), intent(inout), target :: records

    type(anem_nrg_tower_type), pointer :: tower
    type(anem_nrg_record_type), pointer :: record
    type(hash_table_type) local_records
    type(hash_table_iterator_type) record_iterator
    type(datetime_type) time

    character(30) tower_name, time_str
    integer fu, istat, i
    real lon, lat, alt
    real, allocatable :: w_hgt(:)
    real hgt, wd, ws, ws_min, ws_max, ws_std, ta, p, rh

    local_records = hash_table(chunk_size=100000, max_load_factor=0.9)

    call log_notice('Reading ' // trim(file_path) // ' ...')
    fu = unique_file_number()
    open(fu, file=file_path, status='old')
    do while (.true.)
      read(fu, *, iostat=istat) tower_name, hgt, time_str, wd, ws, ws_min, ws_max, ws_std, ta, p, rh
      if (istat < 0) exit
      ! Create tower object if needed.
      if (towers%hashed(tower_name)) then
        select type (value => towers%value(tower_name))
        type is (anem_nrg_tower_type)
          tower => value
        end select
      else
        allocate(tower)
        call query_tower_info(tower_name, lon, lat, alt, w_hgt)
        call tower%init(tower_name, lon, lat, alt)
        tower%seq_id = towers%size
        call towers%insert(tower_name, tower)
      end if
      ! Create record object if needed.
      if (wd /= 99999.0 .and. ws /= 99999.0) then
        time = create_datetime(time_str(1:19), '%Y-%m-%dT%H:%M:%S')
        record => get_record(local_records, time, tower, size(w_hgt))
        record%h = w_hgt
        do i = 1, size(w_hgt)
          if (hgt == w_hgt(i)) exit
        end do
        record%wd_avg(i) = wd
        record%ws_avg(i) = ws
        record%ws_min(i) = ws_min
        record%ws_max(i) = ws_max
        record%ws_std(i) = ws_std
        record%ua_avg(i) = wind_u_component(ws, wd)
        record%va_avg(i) = wind_v_component(ws, wd)
      end if
    end do
    close(fu)

    record_iterator = hash_table_iterator(local_records)
    do while (.not. record_iterator%ended())
      select type (record => record_iterator%value)
      type is (anem_nrg_record_type)
        call records%insert(record)
      end select
      call record_iterator%next()
    end do

    call log_notice('Tower size is ' // trim(to_string(towers%size)) // ', record size is ' // trim(to_string(records%size)) // '.')

  end subroutine anem_txt_read

  subroutine query_tower_info(tower_name, lon, lat, alt, hgt)

    character(*), intent(in) :: tower_name
    real, intent(out) :: lon
    real, intent(out) :: lat
    real, intent(out) :: alt
    real, intent(out), allocatable :: hgt(:)

    integer fu, istat, i
    character(512) line
    type(string_type), allocatable :: hgt_str(:)

    fu = unique_file_number()
    open(fu, file='/data/home/longrun/dongli/tower_info.csv', status='old')
    do while (.true.)
      read(fu, '(A)', iostat=istat) line
      if (istat < 0) then
        call log_error('Cannot find tower ' // trim(tower_name) // '!')
      end if
      if (line(1:len_trim(tower_name)) == tower_name) then
        lon = to_float(split_string(line, ',', 7))
        lat = to_float(split_string(line, ',', 8))
        alt = to_float(split_string(line, ',', 9))
        hgt_str = split_string(split_string(line, ',', 19), ' ')
        if (allocated(hgt) .and. size(hgt) /= size(hgt_str)) deallocate(hgt)
        if (.not. allocated(hgt)) allocate(hgt(size(hgt_str)))
        do i = 1, size(hgt_str)
          read(hgt_str(i)%value, *) hgt(i)
        end do
        return
      end if
    end do
    close(fu)

  end subroutine query_tower_info

  function get_record(records, time, tower, num_level) result(res)

    type(hash_table_type), intent(inout) :: records
    type(datetime_type), intent(in) :: time
    type(anem_nrg_tower_type), intent(in), target :: tower
    integer, intent(in) :: num_level
    type(anem_nrg_record_type), pointer :: res

    if (.not. records%hashed(time%isoformat())) then
      allocate(res)
      res%tower => tower
      res%time = time
      call res%init(num_level)
      call records%insert(time%isoformat(), res)
    end if
    select type (value => records%value(time%isoformat()))
    type is (anem_nrg_record_type)
      res => value
    class default
      call log_error('Internal error!', __FILE__, __LINE__)
    end select

  end function get_record

end module anem_txt_mod
