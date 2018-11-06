module synop_prepbufr_mod

  use datetime
  use synop_mod
  use hash_table_mod
  use linked_list_mod
  use params_mod
  use utils_mod

  implicit none

  private

  public synop_prepbufr_read

  integer, parameter :: max_num_var = 20
  integer, parameter :: max_num_lev = 1
  integer, parameter :: max_num_event = 10

contains

  ! Report types include: 181, 183, 281, 284

  subroutine synop_prepbufr_read(file_path)

    character(*), intent(in) :: file_path

    character(8) subset, station_name
    integer idate, iret, i
    character(10) sdate
    integer msg_count, subset_count
    real(8) hdr(max_num_var)
    real(8) obs(max_num_var,max_num_lev,max_num_event)
    real(8) qc(max_num_var,max_num_lev,max_num_event)
    real(8) pc(max_num_var,max_num_lev,max_num_event)
    type(datetime_type) base_time, time
    logical new_record
    type(synop_station_type), pointer :: station
    type(synop_record_type), pointer :: record

    ! BUFRLIB functions
    integer ireadmg, ireadsb

    stations = hash_table(chunk_size=50000, max_load_factor=0.9)
    call records%clear()

    open(10, file=file_path, action='read', form='unformatted')
    call openbf(10, 'IN', 10)
    call datelen(10) ! This call causes idate to be in format YYYYMMDDHH.
    do while (ireadmg(10, subset, idate) == 0) ! ireadmg returns mnemonic in subset, and copies message into internal arrays.
      msg_count = msg_count + 1
      if (subset /= 'ADPSFC') cycle
      write(sdate, "(I10)") idate
      base_time = create_datetime(sdate, '%Y%m%d%H')
      write(*, "('=> ', I5.5, X, A8)") msg_count, subset
      do while (ireadsb(10) == 0) ! ireadsb copies one subset into internal arrays.
        ! Call values-level subrountines to retrieve actual data values from this subset.
        !                                                                    1   2   3   4   5   6   7   8
        call ufbint(10, hdr, max_num_var, 1,                          iret, 'SID XOB YOB ELV TYP DHR RPT TCOR')
        !                                                                    1   2   3   4   5   6   7    8    9    10   11
        call ufbevn(10, obs, max_num_var, max_num_lev, max_num_event, iret, 'POB TOB QOB TDO UOB VOB TP01 TP03 TP06 TP12 TP24')
        call ufbevn(10, qc,  max_num_var, max_num_lev, max_num_event, iret, 'PQM TQM QQM NUL WQM NUL')
        call ufbevn(10, pc,  max_num_var, max_num_lev, max_num_event, iret, 'PPC TPC QPC NUL WPC NUL')
        station_name = transfer(hdr(1), station_name)
        if (.not. (hdr(5) == 181 .or. hdr(5) == 183 .or. hdr(5) == 281 .or. hdr(5) == 284) .or. len_trim(station_name) == 4) cycle
        time = base_time + timedelta(hours=hdr(6))
        if (stations%hashed(station_name)) then
          select type (value => stations%value(station_name))
          type is (synop_station_type)
            station => value
          end select
        else
          allocate(station)
          station%name = station_name
          station%lon = hdr(2)
          if (station%lon > 180) station%lon = station%lon - 360
          station%lat = hdr(3)
          station%z = hdr(4)
          call stations%insert(station_name, station)
        end if
        nullify(record)
        select type (value => records%last_value())
        type is (synop_record_type)
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
          record%station => station
          record%time = time
          record%type = int(hdr(5))
          new_record = .true.
        end if

        if (is_missing(record%sfc_pressure)) then
          call prepbufr_raw(obs(1,1,:), record%sfc_pressure, stack_qc=qc(1,1,:), stack_pc=pc(1,1,:), qc=record%sfc_pressure_qc)
          ! Convert pressure from hPa to Pa.
          record%sfc_pressure = multiply(record%sfc_pressure, 100.0)
          record%sfc_pressure_stack(:max_num_event) = multiply(prepbufr_stack(obs(1,1,:max_num_event)), 100.0)
          record%sfc_pressure_stack_qc(:max_num_event) = prepbufr_codes(qc(1,1,:max_num_event))
          record%sfc_pressure_stack_pc(:max_num_event) = prepbufr_codes(pc(1,1,:max_num_event))
        end if
        if (is_missing(record%sfc_temperature)) then
          call prepbufr_raw(obs(2,1,:), record%sfc_temperature, stack_qc=qc(2,1,:), stack_pc=pc(2,1,:), qc=record%sfc_temperature_qc)
          record%sfc_temperature_stack(:max_num_event) = prepbufr_stack(obs(2,1,:max_num_event))
          record%sfc_temperature_stack_qc(:max_num_event) = prepbufr_codes(qc(2,1,:max_num_event))
          record%sfc_temperature_stack_pc(:max_num_event) = prepbufr_codes(pc(2,1,:max_num_event))
        end if
        if (is_missing(record%sfc_specific_humidity)) then
          call prepbufr_raw(obs(3,1,:), record%sfc_specific_humidity, stack_qc=qc(3,1,:), stack_pc=pc(3,1,:), qc=record%sfc_specific_humidity_qc)
          record%sfc_specific_humidity_stack(:max_num_event) = prepbufr_stack(obs(3,1,:max_num_event))
          record%sfc_specific_humidity_stack_qc(:max_num_event) = prepbufr_codes(qc(3,1,:max_num_event))
          record%sfc_specific_humidity_stack_pc(:max_num_event) = prepbufr_codes(pc(3,1,:max_num_event))
        end if
        if (is_missing(record%sfc_dewpoint)) then
          call prepbufr_raw(obs(4,1,:), record%sfc_dewpoint)
        end if
        if (is_missing(record%sfc_wind_speed)) then
          call prepbufr_raw(obs(5,1,:), record%sfc_wind_u, stack_qc=qc(5,1,:), stack_pc=pc(5,1,:), qc=record%sfc_wind_qc)
          call prepbufr_raw(obs(6,1,:), record%sfc_wind_v, stack_qc=qc(5,1,:), stack_pc=pc(5,1,:), qc=record%sfc_wind_qc)
          record%sfc_wind_speed     = merge(real_missing_value, sqrt(record%sfc_wind_u**2 + record%sfc_wind_v**2), is_missing(record%sfc_wind_u))
          record%sfc_wind_direction = merge(real_missing_value, wind_direction(record%sfc_wind_u, record%sfc_wind_v), is_missing(record%sfc_wind_u))
          record%sfc_wind_u_stack(:max_num_event) = prepbufr_stack(obs(5,1,:max_num_event))
          record%sfc_wind_v_stack(:max_num_event) = prepbufr_stack(obs(6,1,:max_num_event))
          record%sfc_wind_stack_qc(:max_num_event) = prepbufr_codes(qc(5,1,:max_num_event))
          record%sfc_wind_stack_pc(:max_num_event) = prepbufr_codes(pc(5,1,:max_num_event))
        end if
        if (is_missing(record%sfc_rain_01h)) call prepbufr_raw(obs( 7,1,:), record%sfc_rain_01h)
        if (is_missing(record%sfc_rain_03h)) call prepbufr_raw(obs( 8,1,:), record%sfc_rain_03h)
        if (is_missing(record%sfc_rain_06h)) call prepbufr_raw(obs( 9,1,:), record%sfc_rain_06h)
        if (is_missing(record%sfc_rain_12h)) call prepbufr_raw(obs(10,1,:), record%sfc_rain_12h)
        if (is_missing(record%sfc_rain_24h)) call prepbufr_raw(obs(11,1,:), record%sfc_rain_24h)

        if (new_record) then
          call records%insert(station_name // '@' // time%isoformat(), record)
        ! else
        !   call debug_print(record, hdr, obs, qc, pc)
        end if
      end do
    end do
    call closbf(10)

  end subroutine synop_prepbufr_read

  subroutine debug_print(record, hdr, obs, qc, pc)

    type(synop_record_type), intent(in) :: record
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
    print *, 'W ', record%sfc_wind_speed, record%sfc_wind_qc
    print *, 'W ', record%sfc_wind_direction, record%sfc_wind_qc
    print *, 'W ', record%sfc_wind_u_stack(:4)
    print *, 'W ', record%sfc_wind_v_stack(:4)
    print *, 'W ', record%sfc_wind_stack_qc(:4)
    print *, 'W ', record%sfc_wind_stack_pc(:4)
    print *, 'R ', record%sfc_rain_01h, record%sfc_rain_03h, record%sfc_rain_06h, record%sfc_rain_12h, record%sfc_rain_24h

  end subroutine debug_print

end module synop_prepbufr_mod
