module raob_prepbufr_mod

  use raob_mod
  use datetime
  use hash_table_mod
  use linked_list_mod
  use params_mod
  use utils_mod
  use cli_mod
  use string

  implicit none

  private

  public raob_prepbufr_read

  integer, parameter :: max_num_var   = 20
  integer, parameter :: max_num_lev   = 100
  integer, parameter :: max_num_event = 10

  integer, parameter :: cat_idx   = 1
  integer, parameter :: p_idx     = 2
  integer, parameter :: ta_idx    = 3
  integer, parameter :: sh_idx    = 4
  integer, parameter :: td_idx    = 5
  integer, parameter :: ua_idx    = 6
  integer, parameter :: va_idx    = 7
  integer, parameter :: wd_idx    = 8
  integer, parameter :: ws_idx    = 9
  integer, parameter :: z_idx     = 10

contains

  subroutine raob_prepbufr_read(file_path, stations, records)

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
    real lon, lat, z, p, ta, sh, td, derived_td, rh, ua, va, wd, ws, h
    integer p_qc, ta_qc, sh_qc, ua_qc, va_qc, wd_qc, ws_qc, h_qc
    type(datetime_type) base_time, time
    logical new_record
    type(raob_station_type), pointer :: station
    type(raob_record_type), pointer :: record
    type(linked_list_iterator_type) record_iterator
    type(raob_profile_hash_type), pointer :: profile_hash

    ! BUFRLIB functions
    integer ireadmg, ireadsb

    stations = hash_table(chunk_size=50000, max_load_factor=0.9)
    call records%clear()
    nullify(record)

    write(*, *) '[Notice]: Reading ' // trim(file_path) // ' ...'
    open(10, file=file_path, action='read', form='unformatted')
    call openbf(10, 'IN', 10)
    call datelen(10) ! This call causes idate to be in format YYYYMMDDHH.
    do while (ireadmg(10, subset, idate) == 0) ! ireadmg returns mnemonic in subset, and copies message into internal arrays.
      msg_count = msg_count + 1
      if (subset /= 'ADPUPA') cycle
      write(sdate, "(I10)") idate
      base_time = create_datetime(sdate, '%Y%m%d%H')
      do while (ireadsb(10) == 0) ! ireadsb copies one subset into internal arrays.
        ! Call values-level subrountines to retrieve actual data values from this subset.
        !                                                                    1   2   3   4   5   6   7   8
        call ufbint(10, hdr, max_num_var, 1,                          iret, 'SID XOB YOB ELV TYP DHR RPT TCOR')
        !                                                                    1   2   3   4   5   6   7   8   9   10
        call ufbevn(10, obs, max_num_var, max_num_lev, max_num_event, iret, 'CAT POB TOB QOB TDO UOB VOB DDO FFO ZOB')
        call ufbevn(10, qc,  max_num_var, max_num_lev, max_num_event, iret, 'NUL PQM TQM QQM NUL WQM WQM WQM WQM ZQM')
        call ufbevn(10, pc,  max_num_var, max_num_lev, max_num_event, iret, 'NUL PPC TPC QPC NUL WPC WPC WPC WPC ZPC')
        station_name = transfer(hdr(1), station_name)
        ! Filter out non-RAOB observations.
        if (hdr(5) /= 120 .and. hdr(5) /= 220 .and. hdr(5) /= 132 .and. hdr(5) /= 232 .and. hdr(5) /= 221) cycle
        if (abs(hdr(6)) > 48) then
          write(*, *) '[Error]: DHR is too large with value ', hdr(6), ' at station ', trim(station_name)
          write(*, *) '[Warning]: Use base_time anyway!'
          time = base_time
        else
          time = base_time + create_timedelta(hours=hdr(6))
        end if
        if (stations%hashed(station_name)) then
          select type (value => stations%value(station_name))
          type is (raob_station_type)
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
        type is (raob_record_type)
          ! Since record may be split into two subsets, we need to check if previous record exists with the same time.
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
          select case (int(hdr(5)))
          case (120, 220)
            record%platform_type = 'RAOB'
          case (132, 232)
            record%platform_type = 'DROPSONDE'
          case (221)
            record%platform_type = 'PIBAL'
          end select
          record%station => station
          record%time = time
          new_record = .true.
        end if

        num_level = prepbufr_value_count(obs(cat_idx,:,1))
        do i = 1, num_level
          call prepbufr_raw(obs(p_idx,i,:), p, stack_qc=qc(p_idx,i,:), stack_pc=pc(p_idx,i,:), qc=p_qc)
          if (is_missing(p)) cycle
          key = to_string(int(p))

          select case (int(obs(cat_idx,i,1)))
          case (1) ! Mandatory level
            profile_hash => record%man_hash
          case (2) ! Significant temperature level
            profile_hash => record%sigt_hash
          case (3, 4) ! Winds-by-pressure level or Winds-by-height level
            profile_hash => record%sigw_hash
          case (5) ! Tropopause level
            profile_hash => record%trop_hash
          case (0) ! Surface
            profile_hash => null()
            ! Pressure
            if (is_missing(record%ps)) then
              call prepbufr_raw(obs(p_idx,i,:), record%ps, stack_qc=qc(p_idx,i,:), stack_pc=pc(p_idx,i,:), qc=record%ps_qc)
              record%ps_cr = prepbufr_correct(obs(p_idx,i,:), qc(p_idx,i,:), pc(p_idx,i,:))
            end if
            ! Temperature
            if (is_missing(record%tas)) then
              call prepbufr_raw(obs(ta_idx,i,:), record%tas, stack_qc=qc(ta_idx,i,:), stack_pc=pc(ta_idx,i,:), qc=record%tas_qc)
              record%tas_cr = prepbufr_correct(obs(ta_idx,i,:), qc(ta_idx,i,:), pc(ta_idx,i,:))
            end if
            ! Specific humidity
            if (is_missing(record%shs)) then
              call prepbufr_raw(obs(sh_idx,i,:), record%shs, stack_qc=qc(sh_idx,i,:), stack_pc=pc(sh_idx,i,:), qc=record%shs_qc)
              record%shs_cr = prepbufr_correct(obs(sh_idx,i,:), qc(sh_idx,i,:), pc(sh_idx,i,:))
            end if
            ! Dewpoint
            if (is_missing(record%tds)) then
              call prepbufr_raw(obs(td_idx,i,:), record%tds)
            end if
            ! Wind U component
            if (is_missing(record%uas)) then
              call prepbufr_raw(obs(ua_idx,i,:), record%uas, stack_qc=qc(ua_idx,i,:), stack_pc=pc(ua_idx,i,:), qc=record%uas_qc)
              record%uas_cr = prepbufr_correct(obs(ua_idx,i,:), qc(ua_idx,i,:), pc(ua_idx,i,:))
            end if
            ! Wind V component
            if (is_missing(record%vas)) then
              call prepbufr_raw(obs(va_idx,i,:), record%vas, stack_qc=qc(va_idx,i,:), stack_pc=pc(va_idx,i,:), qc=record%vas_qc)
              record%vas_cr = prepbufr_correct(obs(va_idx,i,:), qc(va_idx,i,:), pc(va_idx,i,:))
            end if
            ! Wind direction
            if (is_missing(record%wds)) then
              call prepbufr_raw(obs(wd_idx,i,:), record%wds, stack_qc=qc(wd_idx,i,:), stack_pc=pc(wd_idx,i,:), qc=record%wds_qc)
            end if
            ! Wind speed
            if (is_missing(record%wss)) then
              call prepbufr_raw(obs(ws_idx,i,:), record%wss, stack_qc=qc(ws_idx,i,:), stack_pc=pc(ws_idx,i,:), qc=record%wss_qc)
              record%wss = knot_to_meter_per_second(record%wss)
            end if
          case default
            write(*, *) '[Warning]: Unknown category ' // to_string(int(obs(cat_idx,i,1))) // ' for station ' // trim(station_name) // '!'
          end select

          if (associated(profile_hash)) then
            ! Pressure
            if (.not. profile_hash%p%hashed(key) .and. .not. is_missing(p)) then
              call profile_hash%p%insert(key, p)
              call profile_hash%p_qc%insert(key, p_qc)
              call profile_hash%p_cr%insert(key, prepbufr_correct(obs(p_idx,i,:), qc(p_idx,i,:), pc(p_idx,i,:)))
            end if
            ! Temperature
            call prepbufr_raw(obs(ta_idx,i,:), ta, stack_qc=qc(ta_idx,i,:), stack_pc=pc(ta_idx,i,:), qc=ta_qc)
            if (.not. profile_hash%ta%hashed(key) .and. .not. is_missing(ta)) then
              call profile_hash%ta%insert(key, ta)
              call profile_hash%ta_qc%insert(key, ta_qc)
              call profile_hash%ta_cr%insert(key, prepbufr_correct(obs(ta_idx,i,:), qc(ta_idx,i,:), pc(ta_idx,i,:)))
            end if
            call prepbufr_raw(obs(sh_idx,i,:), sh, stack_qc=qc(sh_idx,i,:), stack_pc=pc(sh_idx,i,:), qc=sh_qc)
            if (.not. profile_hash%sh%hashed(key) .and. .not. is_missing(sh)) then
              call profile_hash%sh%insert(key, sh)
              call profile_hash%sh_qc%insert(key, sh_qc)
              call profile_hash%sh_cr%insert(key, prepbufr_correct(obs(sh_idx,i,:), qc(sh_idx,i,:), pc(sh_idx,i,:)))
            end if
            ! Dewpoint
            call prepbufr_raw(obs(td_idx,i,:), td)
            derived_td = dewpoint(p, sh)
            if (.not. profile_hash%td%hashed(key)) then
              if (is_missing(td)) then
                td = derived_td
              else if (abs(td - derived_td) > 1.0) then
                td = real_missing_value
              end if
              if (.not. is_missing(td)) call profile_hash%td%insert(key, td)
            end if
            ! Relative humidity
            rh = relative_humidity(p, ta, sh)
            if (.not. profile_hash%rh%hashed(key) .and. .not. is_missing(rh)) then
              call profile_hash%rh%insert(key, rh)
            end if
            ! Wind U component
            call prepbufr_raw(obs(ua_idx,i,:), ua, stack_qc=qc(ua_idx,i,:), stack_pc=pc(ua_idx,i,:), qc=ua_qc)
            if (.not. profile_hash%ua%hashed(key) .and. .not. is_missing(ua)) then
              call profile_hash%ua%insert(key, ua)
              call profile_hash%ua_qc%insert(key, ua_qc)
              call profile_hash%ua_cr%insert(key, prepbufr_correct(obs(ua_idx,i,:), qc(ua_idx,i,:), pc(ua_idx,i,:)))
            end if
            ! Wind V component
            call prepbufr_raw(obs(va_idx,i,:), va, stack_qc=qc(va_idx,i,:), stack_pc=pc(va_idx,i,:), qc=va_qc)
            if (.not. profile_hash%va%hashed(key) .and. .not. is_missing(va)) then
              call profile_hash%va%insert(key, va)
              call profile_hash%va_qc%insert(key, va_qc)
              call profile_hash%va_cr%insert(key, prepbufr_correct(obs(va_idx,i,:), qc(va_idx,i,:), pc(va_idx,i,:)))
            end if
            ! Wind direction
            call prepbufr_raw(obs(wd_idx,i,:), wd, stack_qc=qc(wd_idx,i,:), stack_pc=pc(wd_idx,i,:), qc=wd_qc)
            if (.not. profile_hash%wd%hashed(key) .and. .not. is_missing(wd)) then
              call profile_hash%wd%insert(key, wd)
              call profile_hash%wd_qc%insert(key, wd_qc)
            end if
            ! Wind speed
            call prepbufr_raw(obs(ws_idx,i,:), ws, stack_qc=qc(ws_idx,i,:), stack_pc=pc(ws_idx,i,:), qc=ws_qc)
            if (.not. profile_hash%ws%hashed(key) .and. .not. is_missing(ws)) then
              call profile_hash%ws%insert(key, knot_to_meter_per_second(ws))
              call profile_hash%ws_qc%insert(key, ws_qc)
            end if
            ! Height
            call prepbufr_raw(obs(z_idx,i,:), h, stack_qc=qc(z_idx,i,:), stack_pc=pc(z_idx,i,:), qc=h_qc)
            if (.not. profile_hash%h%hashed(key) .and. .not. is_missing(h)) then
              call profile_hash%h%insert(key, h)
              call profile_hash%h_qc%insert(key, h_qc)
              call profile_hash%h_cr%insert(key, prepbufr_correct(obs(z_idx,i,:), qc(z_idx,i,:), pc(z_idx,i,:)))
            end if
          end if
        end do

        if (new_record) then
          call records%insert(station_name // '@' // time%isoformat(), record)
        end if
      end do
    end do
    call closbf(10)

    ! Transfer read type to final type for easy use.
    num_level = 0
    record_iterator = linked_list_iterator(records)
    do while (.not. record_iterator%ended())
      select type (record => record_iterator%value)
      type is (raob_record_type)
        call record%man %init(record%man_hash %p%size)
        call record%sigt%init(record%sigt_hash%p%size)
        call record%sigw%init(record%sigw_hash%p%size)
        call record%trop%init(record%trop_hash%p%size)
        call record%man %set_from_hash(record%man_hash)
        call record%sigt%set_from_hash(record%sigt_hash)
        call record%sigw%set_from_hash(record%sigw_hash)
        call record%trop%set_from_hash(record%trop_hash)
        call record%station%records%insert(record)
        if (record%station%name == cli_verbose_platform) then
          call record%print()
        end if
        num_level = num_level + record%man%num_level + record%sigt%num_level + record%sigw%num_level + record%trop%num_level
      end select
      call record_iterator%next()
    end do

    write(*, *) '[Notice]: Station size is ' // to_string(stations%size) // ', level size is ' // to_string(num_level) // '.'

  end subroutine raob_prepbufr_read

end module
