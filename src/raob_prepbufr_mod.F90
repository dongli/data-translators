module raob_prepbufr_mod

  use raob_mod
  use datetime
  use hash_table_mod
  use linked_list_mod
  use params_mod
  use utils_mod
  use string_mod

  implicit none

  private

  public raob_prepbufr_read

  integer, parameter :: max_num_var = 20
  integer, parameter :: max_num_lev = 100
  integer, parameter :: max_num_event = 10

  integer, parameter :: cat_idx   = 1
  integer, parameter :: p_idx     = 2
  integer, parameter :: T_idx     = 3
  integer, parameter :: Q_idx     = 4
  integer, parameter :: Td_idx    = 5
  integer, parameter :: u_idx     = 6
  integer, parameter :: v_idx     = 7
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
    real lon, lat, z, p, T, sh, Td, rh, u, v, wd, ws, h
    integer p_qc, T_qc, sh_qc, uv_qc
    type(datetime_type) base_time, time
    logical new_record
    type(raob_station_type), pointer :: station
    type(raob_record_type), pointer :: record
    type(linked_list_iterator_type) record_iterator

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
      ! write(*, "('=> ', I5.5, X, A8)") msg_count, subset
      do while (ireadsb(10) == 0) ! ireadsb copies one subset into internal arrays.
        ! Call values-level subrountines to retrieve actual data values from this subset.
        !                                                                    1   2   3   4   5   6   7   8
        call ufbint(10, hdr, max_num_var, 1,                          iret, 'SID XOB YOB ELV TYP DHR RPT TCOR')
        !                                                                    1   2   3   4   5   6   7   8   9   10
        call ufbevn(10, obs, max_num_var, max_num_lev, max_num_event, iret, 'CAT POB TOB QOB TDO UOB VOB DDO SOB ZOB')
        call ufbevn(10, qc,  max_num_var, max_num_lev, max_num_event, iret, 'NUL PQM TQM QQM NUL WQM WQM WQM WQM ZQM')
        call ufbevn(10, pc,  max_num_var, max_num_lev, max_num_event, iret, 'NUL PPC TPC QPC NUL WPC WPC WPC WPC ZPC')
        station_name = transfer(hdr(1), station_name)
        ! Filter out non-RAOB observations.
        if (.not. (hdr(5) == 120 .or. hdr(5) == 220) .or. len_trim(station_name) /= 5) cycle
        time = base_time + timedelta(hours=hdr(6))
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
          call stations%insert(station_name, station)
        end if
        nullify(record)
        select type (value => records%last_value())
        type is (raob_record_type)
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
          record%station => station
          record%time = time
          new_record = .true.
        end if

        num_level = prepbufr_value_count(obs(cat_idx,:,1))
        do i = 1, num_level
          call prepbufr_raw(obs(p_idx,i,:), p, stack_qc=qc(p_idx,i,:), stack_pc=pc(p_idx,i,:))
          if (is_missing(p)) cycle
          p = p * 100 ! Convert units from hPa to Pa.
          key = to_string(p)
          select case (int(obs(cat_idx,i,1)))
          case (1) ! Mandatory level
            if (.not. record%snd_man_hash%pressure%hashed(key) .and. .not. is_missing(p)) then
              call record%snd_man_hash%pressure%insert(key, p)
            end if
            call prepbufr_raw(obs(T_idx,i,:), T, stack_qc=qc(T_idx,i,:), stack_pc=pc(T_idx,i,:))
            if (.not. record%snd_man_hash%temperature%hashed(key) .and. .not. is_missing(T)) then
              call record%snd_man_hash%temperature%insert(key, T)
            end if
            call prepbufr_raw(obs(Q_idx,i,:), sh, stack_qc=qc(Q_idx,i,:), stack_pc=pc(Q_idx,i,:))
            if (.not. record%snd_man_hash%specific_humidity%hashed(key) .and. .not. is_missing(sh)) then
              call record%snd_man_hash%specific_humidity%insert(key, sh)
            end if
            call prepbufr_raw(obs(Td_idx,i,:), Td)
            if (.not. record%snd_man_hash%dewpoint%hashed(key)) then
              if (is_missing(Td)) Td = dewpoint(p, sh)
              if (.not. is_missing(Td)) call record%snd_man_hash%dewpoint%insert(key, Td)
            end if
            rh = relative_humidity(p, T, sh)
            if (.not. record%snd_man_hash%relative_humidity%hashed(key) .and. .not. is_missing(rh)) then
              call record%snd_man_hash%relative_humidity%insert(key, rh)
            end if
            call prepbufr_raw(obs(u_idx,i,:), u, stack_qc=qc(u_idx,i,:), stack_pc=pc(u_idx,i,:))
            if (.not. record%snd_man_hash%wind_u%hashed(key) .and. .not. is_missing(u)) then
              call record%snd_man_hash%wind_u%insert(key, u)
            end if
            call prepbufr_raw(obs(v_idx,i,:), v, stack_qc=qc(v_idx,i,:), stack_pc=pc(v_idx,i,:))
            if (.not. record%snd_man_hash%wind_v%hashed(key) .and. .not. is_missing(v)) then
              call record%snd_man_hash%wind_v%insert(key, v)
            end if
            call prepbufr_raw(obs(wd_idx,i,:), wd, stack_qc=qc(wd_idx,i,:), stack_pc=pc(wd_idx,i,:))
            if (.not. record%snd_man_hash%wind_direction%hashed(key) .and. .not. is_missing(wd)) then
              call record%snd_man_hash%wind_direction%insert(key, wd)
            end if
            call prepbufr_raw(obs(ws_idx,i,:), ws, stack_qc=qc(ws_idx,i,:), stack_pc=pc(ws_idx,i,:))
            if (.not. record%snd_man_hash%wind_speed%hashed(key) .and. .not. is_missing(ws)) then
              call record%snd_man_hash%wind_speed%insert(key, ws)
            end if
            call prepbufr_raw(obs(z_idx,i,:), h, stack_qc=qc(z_idx,i,:), stack_pc=pc(z_idx,i,:))
            if (.not. record%snd_man_hash%height%hashed(key) .and. .not. is_missing(h)) then
              call record%snd_man_hash%height%insert(key, h)
            end if
          case (2) ! Significant temperature level
            if (.not. record%snd_sig_hash%pressure%hashed(key) .and. .not. is_missing(p)) then
              call record%snd_sig_hash%pressure%insert(key, p)
            end if
            call prepbufr_raw(obs(T_idx,i,:), T, stack_qc=qc(T_idx,i,:), stack_pc=pc(T_idx,i,:))
            if (.not. record%snd_sig_hash%temperature%hashed(key) .and. .not. is_missing(T)) then
              call record%snd_sig_hash%temperature%insert(key, T)
            end if
            call prepbufr_raw(obs(Q_idx,i,:), sh, stack_qc=qc(Q_idx,i,:), stack_pc=pc(Q_idx,i,:))
            if (.not. record%snd_sig_hash%specific_humidity%hashed(key) .and. .not. is_missing(sh)) then
              call record%snd_sig_hash%specific_humidity%insert(key, sh)
            end if
            call prepbufr_raw(obs(Td_idx,i,:), Td, stack_qc=qc(Td_idx,i,:), stack_pc=pc(Td_idx,i,:))
            if (.not. record%snd_sig_hash%dewpoint%hashed(key)) then
              if (is_missing(Td)) Td = dewpoint(p, sh)
              if (.not. is_missing(Td)) call record%snd_sig_hash%dewpoint%insert(key, Td)
            end if
            rh = relative_humidity(p, T, sh)
            if (.not. record%snd_sig_hash%relative_humidity%hashed(key) .and. .not. is_missing(rh)) then
              call record%snd_sig_hash%relative_humidity%insert(key, rh)
            end if
            call prepbufr_raw(obs(u_idx,i,:), u, stack_qc=qc(u_idx,i,:), stack_pc=pc(u_idx,i,:))
            if (.not. record%snd_sig_hash%wind_u%hashed(key) .and. .not. is_missing(u)) then
              call record%snd_sig_hash%wind_u%insert(key, u)
            end if
            call prepbufr_raw(obs(v_idx,i,:), v, stack_qc=qc(v_idx,i,:), stack_pc=pc(v_idx,i,:))
            if (.not. record%snd_sig_hash%wind_v%hashed(key) .and. .not. is_missing(v)) then
              call record%snd_sig_hash%wind_v%insert(key, v)
            end if
            call prepbufr_raw(obs(wd_idx,i,:), wd, stack_qc=qc(wd_idx,i,:), stack_pc=pc(wd_idx,i,:))
            if (.not. record%snd_sig_hash%wind_direction%hashed(key) .and. .not. is_missing(wd)) then
              call record%snd_sig_hash%wind_direction%insert(key, wd)
            end if
            call prepbufr_raw(obs(ws_idx,i,:), ws, stack_qc=qc(ws_idx,i,:), stack_pc=pc(ws_idx,i,:))
            if (.not. record%snd_sig_hash%wind_speed%hashed(key) .and. .not. is_missing(ws)) then
              call record%snd_sig_hash%wind_speed%insert(key, ws)
            end if
            call prepbufr_raw(obs(z_idx,i,:), h, stack_qc=qc(z_idx,i,:), stack_pc=pc(z_idx,i,:))
            if (.not. record%snd_sig_hash%height%hashed(key) .and. .not. is_missing(h)) then
              call record%snd_sig_hash%height%insert(key, h)
            end if
          case (3, 4) ! Winds-by-pressure level or Winds-by-height level
            if (.not. record%snd_wnd_hash%pressure%hashed(key) .and. .not. is_missing(p)) then
              call record%snd_wnd_hash%pressure%insert(key, p)
            end if
            call prepbufr_raw(obs(u_idx,i,:), u, stack_qc=qc(u_idx,i,:), stack_pc=pc(u_idx,i,:))
            if (.not. record%snd_wnd_hash%wind_u%hashed(key) .and. .not. is_missing(u)) then
              call record%snd_wnd_hash%wind_u%insert(key, u)
            end if
            call prepbufr_raw(obs(v_idx,i,:), v, stack_qc=qc(v_idx,i,:), stack_pc=pc(v_idx,i,:))
            if (.not. record%snd_wnd_hash%wind_v%hashed(key) .and. .not. is_missing(v)) then
              call record%snd_wnd_hash%wind_v%insert(key, v)
            end if
            call prepbufr_raw(obs(wd_idx,i,:), wd, stack_qc=qc(wd_idx,i,:), stack_pc=pc(wd_idx,i,:))
            if (.not. record%snd_wnd_hash%wind_direction%hashed(key) .and. .not. is_missing(wd)) then
              call record%snd_wnd_hash%wind_direction%insert(key, wd)
            end if
            call prepbufr_raw(obs(ws_idx,i,:), ws, stack_qc=qc(ws_idx,i,:), stack_pc=pc(ws_idx,i,:))
            if (.not. record%snd_wnd_hash%wind_speed%hashed(key) .and. .not. is_missing(ws)) then
              call record%snd_wnd_hash%wind_speed%insert(key, ws)
            end if
            call prepbufr_raw(obs(z_idx,i,:), h, stack_qc=qc(z_idx,i,:), stack_pc=pc(z_idx,i,:))
            if (.not. record%snd_wnd_hash%height%hashed(key) .and. .not. is_missing(h)) then
              call record%snd_wnd_hash%height%insert(key, h)
            end if
          case (5) ! Tropopause level
            if (.not. record%snd_trop_hash%pressure%hashed(key) .and. .not. is_missing(p)) then
              call record%snd_trop_hash%pressure%insert(key, p)
            end if
            call prepbufr_raw(obs(T_idx,i,:), T, stack_qc=qc(T_idx,i,:), stack_pc=pc(T_idx,i,:))
            if (.not. record%snd_trop_hash%temperature%hashed(key) .and. .not. is_missing(T)) then
              call record%snd_trop_hash%temperature%insert(key, T)
            end if
            call prepbufr_raw(obs(Q_idx,i,:), sh, stack_qc=qc(Q_idx,i,:), stack_pc=pc(Q_idx,i,:))
            if (.not. record%snd_trop_hash%specific_humidity%hashed(key) .and. .not. is_missing(sh)) then
              call record%snd_trop_hash%specific_humidity%insert(key, sh)
            end if
            call prepbufr_raw(obs(Td_idx,i,:), Td, stack_qc=qc(Td_idx,i,:), stack_pc=pc(Td_idx,i,:))
            if (.not. record%snd_trop_hash%dewpoint%hashed(key)) then
              if (is_missing(Td)) Td = dewpoint(p, sh)
              if (.not. is_missing(Td)) call record%snd_trop_hash%dewpoint%insert(key, Td)
            end if
            call prepbufr_raw(obs(u_idx,i,:), u, stack_qc=qc(u_idx,i,:), stack_pc=pc(u_idx,i,:))
            if (.not. record%snd_trop_hash%wind_u%hashed(key) .and. .not. is_missing(u)) then
              call record%snd_trop_hash%wind_u%insert(key, u)
            end if
            call prepbufr_raw(obs(v_idx,i,:), v, stack_qc=qc(v_idx,i,:), stack_pc=pc(v_idx,i,:))
            if (.not. record%snd_trop_hash%wind_v%hashed(key) .and. .not. is_missing(v)) then
              call record%snd_trop_hash%wind_v%insert(key, v)
            end if
            call prepbufr_raw(obs(wd_idx,i,:), wd, stack_qc=qc(wd_idx,i,:), stack_pc=pc(wd_idx,i,:))
            if (.not. record%snd_trop_hash%wind_direction%hashed(key) .and. .not. is_missing(wd)) then
              call record%snd_trop_hash%wind_direction%insert(key, wd)
            end if
            call prepbufr_raw(obs(ws_idx,i,:), ws, stack_qc=qc(ws_idx,i,:), stack_pc=pc(ws_idx,i,:))
            if (.not. record%snd_trop_hash%wind_speed%hashed(key) .and. .not. is_missing(ws)) then
              call record%snd_trop_hash%wind_speed%insert(key, ws)
            end if
            call prepbufr_raw(obs(z_idx,i,:), h, stack_qc=qc(z_idx,i,:), stack_pc=pc(z_idx,i,:))
            if (.not. record%snd_trop_hash%height%hashed(key) .and. .not. is_missing(h)) then
              call record%snd_trop_hash%height%insert(key, h)
            end if
          case (0)
            if (is_missing(record%snd_sfc_pressure)) then
              call prepbufr_raw(obs(p_idx,i,:), record%snd_sfc_pressure, stack_qc=qc(p_idx,i,:), stack_pc=pc(p_idx,i,:))
              record%snd_sfc_pressure = multiply(record%snd_sfc_pressure, 100.0)
            end if
            if (is_missing(record%snd_sfc_temperature)) then
              call prepbufr_raw(obs(T_idx,i,:), record%snd_sfc_temperature, stack_qc=qc(T_idx,i,:), stack_pc=pc(T_idx,i,:))
            end if
            if (is_missing(record%snd_sfc_specific_humidity)) then
              call prepbufr_raw(obs(Q_idx,i,:), record%snd_sfc_specific_humidity, stack_qc=qc(Q_idx,i,:), stack_pc=pc(Q_idx,i,:))
            end if
            if (is_missing(record%snd_sfc_dewpoint)) then
              call prepbufr_raw(obs(Td_idx,i,:), record%snd_sfc_dewpoint)
            end if
            if (is_missing(record%snd_sfc_wind_u)) then
              call prepbufr_raw(obs(u_idx,i,:), record%snd_sfc_wind_u, stack_qc=qc(u_idx,i,:), stack_pc=pc(u_idx,i,:))
            end if
            if (is_missing(record%snd_sfc_wind_v)) then
              call prepbufr_raw(obs(v_idx,i,:), record%snd_sfc_wind_v, stack_qc=qc(v_idx,i,:), stack_pc=pc(v_idx,i,:))
            end if
            if (is_missing(record%snd_sfc_wind_direction)) then
              call prepbufr_raw(obs(wd_idx,i,:), record%snd_sfc_wind_direction, stack_qc=qc(wd_idx,i,:), stack_pc=pc(wd_idx,i,:))
            end if
            if (is_missing(record%snd_sfc_wind_speed)) then
              call prepbufr_raw(obs(ws_idx,i,:), record%snd_sfc_wind_speed, stack_qc=qc(ws_idx,i,:), stack_pc=pc(ws_idx,i,:))
            end if
          case default
            write(*, *) '[Warning]: Unknown category ' // trim(to_string(int(obs(cat_idx,i,1)))) // ' for station ' // trim(station_name) // '!'
            stop
          end select
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
      type is (raob_record_type)
        call record%snd_man %init(record%snd_man_hash %pressure%size)
        call record%snd_sig %init(record%snd_sig_hash %pressure%size)
        call record%snd_wnd %init(record%snd_wnd_hash %pressure%size)
        call record%snd_trop%init(record%snd_trop_hash%pressure%size)
        call record%snd_man %set_from_hash(record%snd_man_hash)
        call record%snd_sig %set_from_hash(record%snd_sig_hash)
        call record%snd_wnd %set_from_hash(record%snd_wnd_hash)
        call record%snd_trop%set_from_hash(record%snd_trop_hash)
        call station%records%insert(record)
        ! if (record%station%name == '70308') then
        !   call debug_print(record, obs, qc, pc)
        ! end if
      end select
      call record_iterator%next()
    end do

    write(*, *) '[Notice]: Station size is ' // trim(to_string(stations%size)) // ', record size is ' // trim(to_string(records%size)) // '.'

  end subroutine raob_prepbufr_read

  subroutine debug_print(record, obs, qc, pc)

    type(raob_record_type), intent(in) :: record
    real(8), intent(in) :: obs(max_num_var,max_num_lev,max_num_event)
    real(8), intent(in) :: qc(max_num_var,max_num_lev,max_num_event)
    real(8), intent(in) :: pc(max_num_var,max_num_lev,max_num_event)

    integer i

    print *, 'Station ', record%station%name
    print *, 'Time ', record%time%isoformat()
    print *, '- Surface:'
    write(*, '(8A15)') 'P', 'T', 'SH', 'TD', 'U', 'V', 'WD', 'WS'
    write(*, '(F15.1)', advance='no') record%snd_sfc_pressure
    write(*, '(F15.1)', advance='no') record%snd_sfc_temperature
    write(*, '(F15.1)', advance='no') record%snd_sfc_specific_humidity
    write(*, '(F15.1)', advance='no') record%snd_sfc_dewpoint
    write(*, '(F15.1)', advance='no') record%snd_sfc_wind_u
    write(*, '(F15.1)', advance='no') record%snd_sfc_wind_v
    write(*, '(F15.1)', advance='no') record%snd_sfc_wind_direction
    write(*, '(F15.1)', advance='no') record%snd_sfc_wind_speed
    write(*, *)
    print *, '- Mandatory levels:'
    write(*, '(10A15)') 'P', 'H', 'T', 'SH', 'TD', 'RH', 'U', 'V', 'WD', 'WS'
    do i = 1, record%snd_man%num_level
      write(*, '(F15.1)', advance='no') record%snd_man%pressure(i)
      write(*, '(F15.1)', advance='no') record%snd_man%height(i)
      write(*, '(F15.1)', advance='no') record%snd_man%temperature(i)
      write(*, '(F15.1)', advance='no') record%snd_man%specific_humidity(i)
      write(*, '(F15.1)', advance='no') record%snd_man%dewpoint(i)
      write(*, '(F15.1)', advance='no') record%snd_man%relative_humidity(i)
      write(*, '(F15.1)', advance='no') record%snd_man%wind_u(i)
      write(*, '(F15.1)', advance='no') record%snd_man%wind_v(i)
      write(*, '(F15.1)', advance='no') record%snd_man%wind_direction(i)
      write(*, '(F15.1)', advance='no') record%snd_man%wind_speed(i)
      write(*, *)
    end do
    print *, '- Significant levels:'
    write(*, '(10A15)') 'P', 'H', 'T', 'SH', 'TD', 'RH', 'U', 'V', 'WD', 'WS'
    do i = 1, record%snd_sig%num_level
      write(*, '(F15.1)', advance='no') record%snd_sig%pressure(i)
      write(*, '(F15.1)', advance='no') record%snd_sig%height(i)
      write(*, '(F15.1)', advance='no') record%snd_sig%temperature(i)
      write(*, '(F15.1)', advance='no') record%snd_sig%specific_humidity(i)
      write(*, '(F15.1)', advance='no') record%snd_sig%dewpoint(i)
      write(*, '(F15.1)', advance='no') record%snd_sig%relative_humidity(i)
      write(*, '(F15.1)', advance='no') record%snd_sig%wind_u(i)
      write(*, '(F15.1)', advance='no') record%snd_sig%wind_v(i)
      write(*, '(F15.1)', advance='no') record%snd_sig%wind_direction(i)
      write(*, '(F15.1)', advance='no') record%snd_sig%wind_speed(i)
      write(*, *)
    end do
    print *, '- Wind levels:'
    write(*, '(2A15, 60X, 4A15)') 'P', 'H', 'U', 'V', 'WD', 'WS'
    do i = 1, record%snd_wnd%num_level
      write(*, '(F15.1)', advance='no') record%snd_wnd%pressure(i)
      write(*, '(F15.1)', advance='no') record%snd_wnd%height(i)
      write(*, '(15X)',   advance='no')
      write(*, '(15X)',   advance='no')
      write(*, '(15X)',   advance='no')
      write(*, '(15X)',   advance='no')
      write(*, '(F15.1)', advance='no') record%snd_wnd%wind_u(i)
      write(*, '(F15.1)', advance='no') record%snd_wnd%wind_v(i)
      write(*, '(F15.1)', advance='no') record%snd_wnd%wind_direction(i)
      write(*, '(F15.1)', advance='no') record%snd_wnd%wind_speed(i)
      write(*, *)
    end do
    print *, '- Tropopause levels:'
    write(*, '(10A15)') 'P', 'H', 'T', 'SH', 'TD', 'RH', 'U', 'V', 'WD', 'WS'
    do i = 1, record%snd_trop%num_level
      write(*, '(F15.1)', advance='no') record%snd_trop%pressure(i)
      write(*, '(F15.1)', advance='no') record%snd_trop%height(i)
      write(*, '(F15.1)', advance='no') record%snd_trop%temperature(i)
      write(*, '(F15.1)', advance='no') record%snd_trop%specific_humidity(i)
      write(*, '(F15.1)', advance='no') record%snd_trop%dewpoint(i)
      write(*, '(F15.1)', advance='no') record%snd_trop%relative_humidity(i)
      write(*, '(F15.1)', advance='no') record%snd_trop%wind_u(i)
      write(*, '(F15.1)', advance='no') record%snd_trop%wind_v(i)
      write(*, '(F15.1)', advance='no') record%snd_trop%wind_direction(i)
      write(*, '(F15.1)', advance='no') record%snd_trop%wind_speed(i)
      write(*, *)
    end do

  end subroutine debug_print

end module
