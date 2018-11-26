module metar_prepbufr_mod

  use datetime
  use metar_mod
  use hash_table_mod
  use linked_list_mod
  use params_mod
  use utils_mod

  implicit none

  private

  public metar_prepbufr_read

  integer, parameter :: max_num_var = 20
  integer, parameter :: max_num_lev = 1
  integer, parameter :: max_num_event = 10

  integer, parameter :: p_idx     = 1
  integer, parameter :: T_idx     = 2
  integer, parameter :: Q_idx     = 3
  integer, parameter :: Td_idx    = 4
  integer, parameter :: u_idx     = 5
  integer, parameter :: v_idx     = 6
  integer, parameter :: wd_idx    = 7
  integer, parameter :: ws_idx    = 8
  integer, parameter :: TP01_idx  = 9
  integer, parameter :: TP03_idx  = 10
  integer, parameter :: TP06_idx  = 11
  integer, parameter :: TP12_idx  = 12
  integer, parameter :: TP24_idx  = 13

contains

  ! Report types include: 181, 183, 281, 284

  subroutine metar_prepbufr_read(file_path, stations, records)

    character(*), intent(in) :: file_path
    type(hash_table_type), intent(inout) :: stations
    type(linked_list_type), intent(inout) :: records

    character(8) subset, station_name
    integer idate, iret, i
    character(10) sdate
    integer msg_count, subset_count
    real(8) hdr(max_num_var)
    real(8) obs(max_num_var,max_num_lev,max_num_event)
    real(8) qc(max_num_var,max_num_lev,max_num_event)
    real(8) pc(max_num_var,max_num_lev,max_num_event)
    type(datetime_type) base_time, time
    real lon, lat, z
    logical new_record
    type(metar_station_type), pointer :: station
    type(metar_record_type), pointer :: record

    ! BUFRLIB functions
    integer ireadmg, ireadsb

    stations = hash_table(chunk_size=50000, max_load_factor=0.9)
    call records%clear()

    write(*, *) '[Notice]: Reading ' // trim(file_path) // ' ...'
    open(10, file=file_path, action='read', form='unformatted')
    call openbf(10, 'IN', 10)
    call datelen(10) ! This call causes idate to be in format YYYYMMDDHH.
    do while (ireadmg(10, subset, idate) == 0) ! ireadmg returns mnemonic in subset, and copies message into internal arrays.
      msg_count = msg_count + 1
      if (subset /= 'ADPSFC') cycle
      write(sdate, "(I10)") idate
      base_time = create_datetime(sdate, '%Y%m%d%H')
      ! write(*, "('=> ', I5.5, X, A8)") msg_count, subset
      do while (ireadsb(10) == 0) ! ireadsb copies one subset into internal arrays.
        ! Call values-level subrountines to retrieve actual data values from this subset.
        !                                                                    1   2   3   4   5   6   7   8
        call ufbint(10, hdr, max_num_var, 1,                          iret, 'SID XOB YOB ELV TYP DHR RPT TCOR')
        !                                                                    1   2   3   4   5   6   7   8   9    10   11   12   13
        call ufbevn(10, obs, max_num_var, max_num_lev, max_num_event, iret, 'POB TOB QOB TDO UOB VOB DDO SOB TP01 TP03 TP06 TP12 TP24')
        call ufbevn(10, qc,  max_num_var, max_num_lev, max_num_event, iret, 'PQM TQM QQM NUL WQM WQM WQM WQM NUL  NUL  NUL  NUL  NUL')
        call ufbevn(10, pc,  max_num_var, max_num_lev, max_num_event, iret, 'PPC TPC QPC NUL WPC WQM WQM WQM NUL  NUL  NUL  NUL  NUL')
        station_name = transfer(hdr(1), station_name)
        station_name = station_name(1:5)
        if (.not. (hdr(5) == 187 .or. hdr(5) == 287) .or. len_trim(station_name) == 4) cycle
        time = base_time + timedelta(hours=hdr(6))
        if (stations%hashed(station_name)) then
          select type (value => stations%value(station_name))
          type is (metar_station_type)
            station => value
          end select
        else
          allocate(station)
          lon = hdr(2)
          if (.not. is_missing(lon) .and. lon > 180) lon = lon - 360
          lat = hdr(3)
          z = hdr(4)
          call station%init(station_name, lon, lat, z)
          call stations%insert(station_name, station)
        end if
        nullify(record)
        select type (value => records%last_value())
        type is (metar_record_type)
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
          record%seq_id = records%size
          record%station => station
          record%time = time
          record%type = int(hdr(5))
          new_record = .true.
        end if

        if (is_missing(record%sfc_pressure)) then
          call prepbufr_raw(obs(p_idx,1,:), record%sfc_pressure, stack_qc=qc(p_idx,1,:), stack_pc=pc(p_idx,1,:), qc=record%sfc_pressure_qc)
          record%sfc_pressure = multiply(record%sfc_pressure, 100.0)
          record%sfc_pressure_stack(:max_num_event) = multiply(prepbufr_stack(obs(p_idx,1,:max_num_event)), 100.0)
          record%sfc_pressure_stack_qc(:max_num_event) = prepbufr_codes(qc(p_idx,1,:max_num_event))
          record%sfc_pressure_stack_pc(:max_num_event) = prepbufr_codes(pc(p_idx,1,:max_num_event))
        end if
        if (is_missing(record%sfc_temperature)) then
          call prepbufr_raw(obs(T_idx,1,:), record%sfc_temperature, stack_qc=qc(T_idx,1,:), stack_pc=pc(T_idx,1,:), qc=record%sfc_temperature_qc)
          record%sfc_temperature_stack(:max_num_event) = prepbufr_stack(obs(T_idx,1,:max_num_event))
          record%sfc_temperature_stack_qc(:max_num_event) = prepbufr_codes(qc(T_idx,1,:max_num_event))
          record%sfc_temperature_stack_pc(:max_num_event) = prepbufr_codes(pc(T_idx,1,:max_num_event))
        end if
        if (is_missing(record%sfc_specific_humidity)) then
          call prepbufr_raw(obs(Q_idx,1,:), record%sfc_specific_humidity, stack_qc=qc(Q_idx,1,:), stack_pc=pc(Q_idx,1,:), qc=record%sfc_specific_humidity_qc)
          record%sfc_specific_humidity_stack(:max_num_event) = prepbufr_stack(obs(Q_idx,1,:max_num_event))
          record%sfc_specific_humidity_stack_qc(:max_num_event) = prepbufr_codes(qc(Q_idx,1,:max_num_event))
          record%sfc_specific_humidity_stack_pc(:max_num_event) = prepbufr_codes(pc(Q_idx,1,:max_num_event))
        end if
        if (is_missing(record%sfc_dewpoint)) then
          call prepbufr_raw(obs(Td_idx,1,:), record%sfc_dewpoint)
        end if
        if (is_missing(record%sfc_wind_u)) then
          call prepbufr_raw(obs(u_idx,1,:), record%sfc_wind_u, stack_qc=qc(u_idx,1,:), stack_pc=pc(u_idx,1,:), qc=record%sfc_wind_qc)
          record%sfc_wind_u_stack(:max_num_event) = prepbufr_stack(obs(u_idx,1,:max_num_event))
          record%sfc_wind_stack_qc(:max_num_event) = prepbufr_codes(qc(u_idx,1,:max_num_event))
        end if
        if (is_missing(record%sfc_wind_v)) then
          call prepbufr_raw(obs(v_idx,1,:), record%sfc_wind_v, stack_qc=qc(v_idx,1,:), stack_pc=pc(v_idx,1,:), qc=record%sfc_wind_qc)
          record%sfc_wind_v_stack(:max_num_event) = prepbufr_stack(obs(v_idx,1,:max_num_event))
          record%sfc_wind_stack_pc(:max_num_event) = prepbufr_codes(pc(v_idx,1,:max_num_event))
        end if
        if (is_missing(record%sfc_wind_direction)) then
          call prepbufr_raw(obs(wd_idx,1,:), record%sfc_wind_direction, stack_qc=qc(wd_idx,1,:), stack_pc=pc(wd_idx,1,:), qc=record%sfc_wind_qc)
        end if
        if (is_missing(record%sfc_wind_speed)) then
          call prepbufr_raw(obs(ws_idx,1,:), record%sfc_wind_speed, stack_qc=qc(ws_idx,1,:), stack_pc=pc(ws_idx,1,:), qc=record%sfc_wind_qc)
        end if
        if (is_missing(record%sfc_rain_01h)) call prepbufr_raw(obs(TP01_idx,1,:), record%sfc_rain_01h)
        if (is_missing(record%sfc_rain_03h)) call prepbufr_raw(obs(TP03_idx,1,:), record%sfc_rain_03h)
        if (is_missing(record%sfc_rain_06h)) call prepbufr_raw(obs(TP06_idx,1,:), record%sfc_rain_06h)
        if (is_missing(record%sfc_rain_12h)) call prepbufr_raw(obs(TP12_idx,1,:), record%sfc_rain_12h)
        if (is_missing(record%sfc_rain_24h)) call prepbufr_raw(obs(TP24_idx,1,:), record%sfc_rain_24h)

        if (new_record) then
          call records%insert(station_name // '@' // time%isoformat(), record)
        ! else
        !   call debug_print(record, hdr, obs, qc, pc)
        end if
        call station%records%insert(trim(to_string(record%seq_id)), record, nodup=.true.)
      end do
    end do
    call closbf(10)

    write(*, *) '[Notice]: Station size is ' // trim(to_string(stations%size)) // ', record size is ' // trim(to_string(records%size)) // '.'

  end subroutine metar_prepbufr_read

  subroutine debug_print(record, hdr, obs, qc, pc)

    type(metar_record_type), intent(in) :: record
    real(8), intent(in) :: hdr(max_num_var)
    real(8), intent(in) :: obs(max_num_var,max_num_lev,max_num_event)
    real(8), intent(in) :: qc(max_num_var,max_num_lev,max_num_event)
    real(8), intent(in) :: pc(max_num_var,max_num_lev,max_num_event)

    print *, '--'
    print *, record%station%name, record%time%isoformat(), hdr(6), hdr(7)
    print *, record%station%lon, record%station%lat, record%station%z
    print *, 'T ', record%sfc_temperature, record%sfc_temperature_qc
    print *, 'T ', record%sfc_temperature_stack(:4)
    print *, 'T ', record%sfc_temperature_stack_qc(:4)
    print *, 'T ', record%sfc_temperature_stack_pc(:4)
    print *, 'Q ', record%sfc_specific_humidity, record%sfc_specific_humidity_qc
    print *, 'Q ', record%sfc_specific_humidity_stack(:4)
    print *, 'Q ', record%sfc_specific_humidity_stack_qc(:4)
    print *, 'Q ', record%sfc_specific_humidity_stack_pc(:4)
    print *, 'TD', record%sfc_dewpoint
    print *, 'P ', record%sfc_pressure, record%sfc_pressure_qc
    print *, 'P ', record%sfc_pressure_stack(:4)
    print *, 'P ', record%sfc_pressure_stack_qc(:4)
    print *, 'P ', record%sfc_pressure_stack_pc(:4)
    print *, 'WS', record%sfc_wind_speed, record%sfc_wind_qc
    print *, 'WD', record%sfc_wind_direction, record%sfc_wind_qc
    print *, 'W ', record%sfc_wind_u_stack(:4)
    print *, 'W ', record%sfc_wind_v_stack(:4)
    print *, 'W ', record%sfc_wind_stack_qc(:4)
    print *, 'W ', record%sfc_wind_stack_pc(:4)
    print *, 'R ', record%sfc_rain_01h, record%sfc_rain_03h, record%sfc_rain_06h, record%sfc_rain_12h, record%sfc_rain_24h

  end subroutine debug_print

end module metar_prepbufr_mod
