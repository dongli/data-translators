module anem_nrg_txt_mod

  use anem_nrg_mod
  use datetime
  use string
  use timedelta_mod
  use hash_table_mod
  use linked_list_mod
  use regex
  use params_mod
  use utils_mod
  use cli_mod

  implicit none

  private

  public anem_nrg_txt_read

contains

  subroutine anem_nrg_txt_read(file_path_list, towers, records)

    character(*), intent(in) :: file_path_list
    type(hash_table_type), intent(inout), target :: towers
    type(linked_list_type), intent(inout), target :: records

    type(string_type), allocatable :: file_paths(:)
    type(linked_list_type) dummy_records
    type(linked_list_iterator_type) dummy_record_iterator
    type(anem_nrg_tower_type), pointer :: tower
    type(anem_nrg_record_type), pointer :: record
    type(string_type), allocatable :: labels(:)
    type(string_type), allocatable :: fields(:)
    type(reg_matches), allocatable :: matches(:)
    type(datetime_type) time

    integer :: wd_avg_idx(10) = 0
    integer :: ws_avg_idx(10) = 0
    integer :: wd_std_idx(10) = 0
    integer :: ws_std_idx(10) = 0
    integer :: wd_min_idx(10) = 0
    integer :: ws_min_idx(10) = 0
    integer :: wd_max_idx(10) = 0
    integer :: ws_max_idx(10) = 0
    integer :: ta_avg_idx(10) = 0
    integer :: ta_std_idx(10) = 0
    integer :: ta_min_idx(10) = 0
    integer :: ta_max_idx(10) = 0
    integer :: p_avg_idx (10) = 0
    integer :: p_std_idx (10) = 0
    integer :: p_min_idx (10) = 0
    integer :: p_max_idx (10) = 0

    character(2048) line
    character(6) tower_id
    character(8) timezone
    real lon, lat, z
    real h(10)
    integer i, j, k, n, iostat

    towers = hash_table(chunk_size=1000, max_load_factor=0.9)
    call records%clear()

    file_paths = split_string(file_path_list, ',')

    do k = 1, size(file_paths)
      write(*, *) '[Notice]: Reading ' // trim(file_paths(k)%value) // ' ...'
      open(10, file=file_paths(k)%value, status='old')
      ! Read header.
      do while (.true.)
        read(10, '(A2048)') line
        if (line(1:9) == 'Timestamp') then
          labels = split_string(line, char(9))
          exit
        end if
        if (line(1:11) == 'Site Number') then
          read(line(14:), *) tower_id
        else if (line(1:8) == 'Latitude') then
          read(line(11:), *) lat
        else if (line(1:9) == 'Longitude') then
          read(line(11:), *) lon
        else if (line(1:9) == 'Elevation') then
          read(line(11:), *) z
        else if (line(1:9) == 'Time Zone') then
          read(line(12:), *) timezone
        end if
      end do
      if (towers%hashed(tower_id)) then
        select type (value => towers%value(tower_id))
        type is (anem_nrg_tower_type)
          tower => value
        end select
      else
        allocate(tower)
        call tower%init(tower_id, lon, lat, z)
        call towers%insert(tower_id, tower)
      end if
      ! Get heights from labels.
      n = 0
      loop_labels: do i = 2, size(labels)
        matches = regex_search(labels(i)%value, '(\d+\.\d+)m')
        if (size(matches) == 0) then
          write(*, *) '[Error]: Failed to find height from column label ' // trim(labels(i)%value) // '!'
          stop 1
        end if
        read(matches(1)%match(2)%str, *) h(n+1)
        deallocate(matches)
        do j = 1, n
          if (h(j) == h(n+1)) cycle loop_labels
        end do
        n = n + 1
      end do loop_labels
      ! Set indices for needed variables.
      ws_avg_idx = 0; ws_std_idx = 0; ws_min_idx = 0; ws_max_idx = 0
      wd_avg_idx = 0; wd_std_idx = 0; wd_min_idx = 0; wd_max_idx = 0
      ta_avg_idx = 0; ta_std_idx = 0; ta_min_idx = 0; ta_max_idx = 0
       p_avg_idx = 0;  p_std_idx = 0;  p_min_idx = 0;  p_max_idx = 0
      do j = 1, n
        do i = 2, size(labels)
          if (index(labels(i)%value, to_string(int(h(j)))) == 0) cycle
          if (index(labels(i)%value, 'Avg_m/s') /= 0) then
            ws_avg_idx(j) = i
          else if (index(labels(i)%value, 'SD_m/s') /= 0) then
            ws_std_idx(j) = i
          else if (index(labels(i)%value, 'Min_m/s') /= 0) then
            ws_min_idx(j) = i
          else if (index(labels(i)%value, 'Max_m/s') /= 0) then
            ws_max_idx(j) = i
          else if (index(labels(i)%value, 'Avg_deg') /= 0 .or. index(labels(i)%value, 'Avg_Deg') /= 0) then
            wd_avg_idx(j) = i
          else if (index(labels(i)%value, 'SD_deg' ) /= 0 .or. index(labels(i)%value, 'SD_Deg' ) /= 0) then
            wd_std_idx(j) = i
          else if (index(labels(i)%value, 'Min_deg') /= 0 .or. index(labels(i)%value, 'Min_Deg') /= 0) then
            wd_min_idx(j) = i
          else if (index(labels(i)%value, 'Max_deg') /= 0 .or. index(labels(i)%value, 'Max_Deg') /= 0) then
            wd_max_idx(j) = i
          else if (index(labels(i)%value, 'Avg_C') /= 0) then
            ta_avg_idx(j) = i
          else if (index(labels(i)%value, 'SD_C') /= 0) then
            ta_std_idx(j) = i
          else if (index(labels(i)%value, 'Min_C') /= 0) then
            ta_min_idx(j) = i
          else if (index(labels(i)%value, 'Max_C') /= 0) then
            ta_max_idx(j) = i
          else if (index(labels(i)%value, 'Avg_kPa') /= 0) then
            p_avg_idx(j) = i
          else if (index(labels(i)%value, 'SD_kPa') /= 0) then
            p_std_idx(j) = i
          else if (index(labels(i)%value, 'Min_kPa') /= 0) then
            p_min_idx(j) = i
          else if (index(labels(i)%value, 'Max_kPa') /= 0) then
            p_max_idx(j) = i
          end if
        end do
      end do
      do while (.true.)
        read(10, '(A2048)', iostat=iostat) line
        if (iostat < 0) exit
        fields = split_string(line, char(9))
        time = create_datetime(fields(1)%value, '%Y-%m-%d %H:%M:%S')
        allocate(record)
        call record%init(n)
        record%seq_id = records%size
        record%tower => tower
        record%time = time
        do i = 1, n
          record%h(i) = h(i)
          if (ws_avg_idx(i) /= 0 .and. ws_avg_idx(i) < size(fields)) then
            if (fields(ws_avg_idx(i))%value /= '') record%ws_avg(i) = to_float(fields(ws_avg_idx(i))%value)
          end if
          if (ws_std_idx(i) /= 0 .and. ws_std_idx(i) < size(fields)) then
            if (fields(ws_std_idx(i))%value /= '') record%ws_std(i) = to_float(fields(ws_std_idx(i))%value)
          end if
          if (ws_min_idx(i) /= 0 .and. ws_min_idx(i) < size(fields)) then
            if (fields(ws_min_idx(i))%value /= '') record%ws_min(i) = to_float(fields(ws_min_idx(i))%value)
          end if
          if (ws_max_idx(i) /= 0 .and. ws_max_idx(i) < size(fields)) then
            if (fields(ws_max_idx(i))%value /= '') record%ws_max(i) = to_float(fields(ws_max_idx(i))%value)
          end if
          if (wd_avg_idx(i) /= 0 .and. wd_avg_idx(i) < size(fields)) then
            if (fields(wd_avg_idx(i))%value /= '') record%wd_avg(i) = to_float(fields(wd_avg_idx(i))%value)
          end if
          if (wd_std_idx(i) /= 0 .and. wd_std_idx(i) < size(fields)) then
            if (fields(wd_std_idx(i))%value /= '') record%wd_std(i) = to_float(fields(wd_std_idx(i))%value)
          end if
          if (wd_min_idx(i) /= 0 .and. wd_min_idx(i) < size(fields)) then
            if (fields(wd_min_idx(i))%value /= '') record%wd_min(i) = to_float(fields(wd_min_idx(i))%value)
          end if
          if (wd_max_idx(i) /= 0 .and. wd_max_idx(i) < size(fields)) then
            if (fields(wd_max_idx(i))%value /= '') record%wd_max(i) = to_float(fields(wd_max_idx(i))%value)
          end if
          if (ta_avg_idx(i) /= 0 .and. ta_avg_idx(i) < size(fields)) then
            if (fields(ta_avg_idx(i))%value /= '') record%ta_avg(i) = to_float(fields(ta_avg_idx(i))%value)
          end if
          if (ta_std_idx(i) /= 0 .and. ta_std_idx(i) < size(fields)) then
            if (fields(ta_std_idx(i))%value /= '') record%ta_std(i) = to_float(fields(ta_std_idx(i))%value)
          end if
          if (ta_min_idx(i) /= 0 .and. ta_min_idx(i) < size(fields)) then
            if (fields(ta_min_idx(i))%value /= '') record%ta_min(i) = to_float(fields(ta_min_idx(i))%value)
          end if
          if (ta_max_idx(i) /= 0 .and. ta_max_idx(i) < size(fields)) then
            if (fields(ta_max_idx(i))%value /= '') record%ta_max(i) = to_float(fields(ta_max_idx(i))%value)
          end if
          if (p_avg_idx(i)  /= 0 .and. p_avg_idx(i)  < size(fields)) then
            if (fields(p_avg_idx (i))%value /= '') record%p_avg(i)  = multiply(to_float(fields(p_avg_idx (i))%value), 10.0)
          end if
          if (p_std_idx(i)  /= 0 .and. p_std_idx(i)  < size(fields)) then
            if (fields(p_std_idx (i))%value /= '') record%p_std(i)  = multiply(to_float(fields(p_std_idx (i))%value), 10.0)
          end if
          if (p_min_idx(i)  /= 0 .and. p_min_idx(i)  < size(fields)) then
            if (fields(p_min_idx (i))%value /= '') record%p_min(i)  = multiply(to_float(fields(p_min_idx (i))%value), 10.0)
          end if
          if (p_max_idx(i)  /= 0 .and. p_max_idx(i)  < size(fields)) then
            if (fields(p_max_idx (i))%value /= '') record%p_max(i)  = multiply(to_float(fields(p_max_idx (i))%value), 10.0)
          end if
          if (ws_avg_idx(i) /= 0 .and. wd_avg_idx(i) /= 0) then
            record%ua_avg(i) = wind_u_component(record%ws_avg(i), record%wd_avg(i))
            record%va_avg(i) = wind_v_component(record%ws_avg(i), record%wd_avg(i))
          end if
        end do
        call dummy_records%insert(tower_id // '@' // time%isoformat(), record)
        call tower%records%insert(trim(to_string(record%seq_id)), record, nodup=.true.)
        deallocate(fields)
      end do
      close(10)

      deallocate(labels)
    end do

    dummy_record_iterator = linked_list_iterator(dummy_records)
    do while (.not. dummy_record_iterator%ended())
      select type (record => dummy_record_iterator%value)
      type is (anem_nrg_record_type)
        if (cli_start_time%year == 0 .or. (cli_start_time <= record%time .and. record%time <= cli_end_time)) then
          call records%insert(record)
        end if
      end select
      call dummy_record_iterator%next()
    end do

    write(*, *) '[Notice]: Tower size is ' // trim(to_string(towers%size)) // ', record size is ' // trim(to_string(records%size)) // '.'

  end subroutine anem_nrg_txt_read

end module anem_nrg_txt_mod
