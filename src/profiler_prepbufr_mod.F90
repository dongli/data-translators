module profiler_prepbufr_mod

  use datetime
  use string
  use container
  use flogger
  use params_mod
  use data_translators_utils_mod
  use cli_mod
  use profiler_mod

  implicit none

  private

  public profiler_prepbufr_read

  integer, parameter :: max_num_var = 10
  integer, parameter :: max_num_lev = 100
  integer, parameter :: max_num_event = 10

  integer, parameter :: cat_idx   = 1
  integer, parameter :: p_idx     = 2
  integer, parameter :: z_idx     = 3
  integer, parameter :: ua_idx    = 4
  integer, parameter :: va_idx    = 5
  integer, parameter :: wd_idx    = 6
  integer, parameter :: ws_idx    = 7

contains

  subroutine profiler_prepbufr_read(file_path, stations, records)

    character(*), intent(in) :: file_path
    type(hash_table_type), intent(inout) :: stations
    type(linked_list_type), intent(inout) :: records

    character(8) subset, station_name
    integer idate, iret, i
    character(10) sdate
    character(20) key
    integer msg_count, subset_count, num_level
    real(8) hdr(max_num_var)
    real(8) obs(max_num_var,max_num_lev,max_num_event)
    real(8) qc(max_num_var,max_num_lev,max_num_event)
    real(8) pc(max_num_var,max_num_lev,max_num_event)
    real lon, lat, z, p, h, ua, va, wd, ws
    integer p_qc, h_qc, ua_qc, va_qc, wd_qc, ws_qc
    type(datetime_type) base_time, time
    logical new_record
    type(profiler_station_type), pointer :: station
    type(profiler_record_type), pointer :: record
    type(linked_list_iterator_type) record_iterator

    ! BUFRLIB functions
    integer ireadmg, ireadsb

    call log_notice('Reading ' // trim(file_path) // ' ...')
    open(10, file=file_path, action='read', form='unformatted')
    call openbf(10, 'IN', 10)
    call datelen(10) ! This call causes idate to be in format YYYYMMDDHH.
    do while (ireadmg(10, subset, idate) == 0) ! ireadmg returns mnemonic in subset, and copies message into internal arrays.
      msg_count = msg_count + 1
      if (subset /= 'PROFLR') cycle
      write(sdate, "(I10)") idate
      base_time = create_datetime(sdate, '%Y%m%d%H')
      do while (ireadsb(10) == 0) ! ireadsb copies one subset into internal arrays.
        ! Call values-level subrountines to retrieve actual data values from this subset.
        !                                                                    1   2   3   4   5   6   7   8
        call ufbint(10, hdr, max_num_var, 1,                          iret, 'SID XOB YOB ELV TYP DHR RPT TCOR')
        !                                                                    1   2   3   4   5   6   7
        call ufbevn(10, obs, max_num_var, max_num_lev, max_num_event, iret, 'CAT POB ZOB UOB VOB DDO FFO')
        call ufbevn(10, qc,  max_num_var, max_num_lev, max_num_event, iret, 'NUL PQM ZQM WQM WQM DFQ NUL')
        call ufbevn(10, pc,  max_num_var, max_num_lev, max_num_event, iret, 'NUL PPC ZPC WPC WPC DFP NUL')
        station_name = transfer(hdr(1), station_name)
        ! Filter out non-profiler observations.
        if (hdr(5) /= 223 .and. hdr(5) /= 227 .and. hdr(5) /= 228 .and. hdr(5) /= 229) cycle
        time = base_time + create_timedelta(hours=hdr(6))
        if (stations%hashed(station_name)) then
          select type (value => stations%value(station_name))
          type is (profiler_station_type)
            station => value
          end select
        else
          allocate(station)
          lon = hdr(2)
          if (lon > 180) lon = lon - 360
          lat = hdr(3)
          z = hdr(4)
          call station%init(station_name, lon, lat, z)
          station%seq_id = stations%size
          call stations%insert(station_name, station)
        end if
        nullify(record)
        select type (value => records%last_value())
        type is (profiler_record_type)
          ! Since recode may be split into two subsets, we need to check if previous record exists with the same time.
          record => value
          if (record%station%name == station_name .and. record%time == time) then
            new_record = .false.
          else
            nullify(record)
          end if
        end select
        if (.not. associated(record)) then
          allocate(record)
          call record%init(alloc_hash=.true.)
          record%seq_id = records%size
          record%station => station
          record%time = time
          new_record = .true.
        end if

        num_level = prepbufr_value_count(obs(cat_idx,:,1))
        do i = 1, num_level
          call prepbufr_raw(obs(p_idx,i,:), p, stack_qc=qc(p_idx,i,:), stack_pc=pc(p_idx,i,:), qc=p_qc)
          if (is_missing(p)) cycle
          p = p * 100 ! Convert units from hPa to Pa.
          key = to_str(int(p))
          if (.not. record%pro_hash%p%hashed(key) .and. .not. is_missing(p)) then
            call record%pro_hash%p%insert(key, p)
            call record%pro_hash%p_qc%insert(key, p_qc)
            call record%pro_hash%p_cr%insert(key, prepbufr_correct(obs(p_idx,i,:), qc(p_idx,i,:), pc(p_idx,i,:)))
          end if
          call prepbufr_raw(obs(z_idx,i,:), h, stack_qc=qc(z_idx,i,:), stack_pc=pc(z_idx,i,:), qc=h_qc)
          if (.not. record%pro_hash%h%hashed(key) .and. .not. is_missing(h)) then
            call record%pro_hash%h%insert(key, h)
            call record%pro_hash%h_qc%insert(key, h_qc)
            call record%pro_hash%h_cr%insert(key, prepbufr_correct(obs(z_idx,i,:), qc(z_idx,i,:), pc(z_idx,i,:)))
          end if
          call prepbufr_raw(obs(ua_idx,i,:), ua, stack_qc=qc(ua_idx,i,:), stack_pc=pc(ua_idx,i,:), qc=ua_qc)
          if (.not. record%pro_hash%ua%hashed(key) .and. .not. is_missing(ua)) then
            call record%pro_hash%ua%insert(key, ua)
            call record%pro_hash%ua_qc%insert(key, ua_qc)
            call record%pro_hash%ua_cr%insert(key, prepbufr_correct(obs(ua_idx,i,:), qc(ua_idx,i,:), pc(ua_idx,i,:)))
          end if
          call prepbufr_raw(obs(va_idx,i,:), va, stack_qc=qc(va_idx,i,:), stack_pc=pc(va_idx,i,:), qc=va_qc)
          if (.not. record%pro_hash%va%hashed(key) .and. .not. is_missing(va)) then
            call record%pro_hash%va%insert(key, va)
            call record%pro_hash%va_qc%insert(key, va_qc)
            call record%pro_hash%va_cr%insert(key, prepbufr_correct(obs(va_idx,i,:), qc(va_idx,i,:), pc(va_idx,i,:)))
          end if
          call prepbufr_raw(obs(wd_idx,i,:), wd, stack_qc=qc(wd_idx,i,:), stack_pc=pc(wd_idx,i,:), qc=wd_qc)
          if (.not. record%pro_hash%wd%hashed(key) .and. .not. is_missing(wd)) then
            call record%pro_hash%wd%insert(key, wd)
            call record%pro_hash%wd_qc%insert(key, wd_qc)
          end if
          call prepbufr_raw(obs(ws_idx,i,:), ws, stack_qc=qc(ws_idx,i,:), stack_pc=pc(ws_idx,i,:), qc=ws_qc)
          if (.not. record%pro_hash%ws%hashed(key) .and. .not. is_missing(ws)) then
            call record%pro_hash%ws%insert(key, ws)
            call record%pro_hash%ws_qc%insert(key, ws_qc)
          end if
        end do

        if (new_record) then
          call records%insert(station_name // '@' // time%isoformat(), record)
        end if
      end do
    end do
    call closbf(10)

    ! Transfer read type to final type for easy use.
    record_iterator = linked_list_iterator(records)
    do while (.not. record_iterator%ended())
      select type (record => record_iterator%value)
      type is (profiler_record_type)
        call record%pro%init(record%pro_hash%p%size)
        if (associated(record%pro_hash)) then
          call record%pro%set_from_hash(record%pro_hash)
          call record%station%records%insert(record)
          if (record%station%name == cli_verbose_platform) then
            call record%print()
          end if
        end if
      end select
      call record_iterator%next()
    end do

    call log_notice('Station size is ' // to_str(stations%size) // ', record size is ' // to_str(records%size) // '.')

  end subroutine profiler_prepbufr_read

end module profiler_prepbufr_mod
