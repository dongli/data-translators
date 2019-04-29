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
  public metar_prepbufr_write

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
      do while (ireadsb(10) == 0) ! ireadsb copies one subset into internal arrays.
        ! Call values-level subrountines to retrieve actual data values from this subset.
        !                                                                    1   2   3   4   5   6   7   8
        call ufbint(10, hdr, max_num_var, 1,                          iret, 'SID XOB YOB ELV TYP DHR RPT TCOR')
        !                                                                    1   2   3   4   5   6   7   8   9    10   11   12   13
        call ufbevn(10, obs, max_num_var, max_num_lev, max_num_event, iret, 'POB TOB QOB TDO UOB VOB DDO SOB TP01 TP03 TP06 TP12 TP24')
        call ufbevn(10, qc,  max_num_var, max_num_lev, max_num_event, iret, 'PQM TQM QQM NUL WQM WQM WQM WQM NUL  NUL  NUL  NUL  NUL')
        call ufbevn(10, pc,  max_num_var, max_num_lev, max_num_event, iret, 'PPC TPC QPC NUL WPC WPC WPC WPC NUL  NUL  NUL  NUL  NUL')
        station_name = transfer(hdr(1), station_name)
        station_name = station_name(1:5)
        if (hdr(5) /= 187 .and. hdr(5) /= 287) cycle
        time = base_time + timedelta(hours=hdr(6))
        if (stations%hashed(station_name)) then
          select type (value => stations%value(station_name))
          type is (metar_station_type)
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
        type is (metar_record_type)
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
          record%seq_id = records%size
          record%station => station
          record%time = time
          record%type = int(hdr(5))
          new_record = .true.
        end if

        if (is_missing(record%pressure)) then
          call prepbufr_raw(obs(p_idx,1,:), record%pressure, stack_qc=qc(p_idx,1,:), stack_pc=pc(p_idx,1,:), qc=record%pressure_qc)
          record%pressure = multiply(record%pressure, 100.0) ! Convert pressure from hPa to Pa.
          record%pressure_stack(:max_num_event) = multiply(prepbufr_stack(obs(p_idx,1,:max_num_event)), 100.0)
          record%pressure_stack_qc(:max_num_event) = prepbufr_codes(qc(p_idx,1,:max_num_event))
          record%pressure_stack_pc(:max_num_event) = prepbufr_codes(pc(p_idx,1,:max_num_event))
        end if
        if (is_missing(record%temperature)) then
          call prepbufr_raw(obs(T_idx,1,:), record%temperature, stack_qc=qc(T_idx,1,:), stack_pc=pc(T_idx,1,:), qc=record%temperature_qc)
          record%temperature_stack(:max_num_event) = prepbufr_stack(obs(T_idx,1,:max_num_event))
          record%temperature_stack_qc(:max_num_event) = prepbufr_codes(qc(T_idx,1,:max_num_event))
          record%temperature_stack_pc(:max_num_event) = prepbufr_codes(pc(T_idx,1,:max_num_event))
        end if
        if (is_missing(record%specific_humidity)) then
          call prepbufr_raw(obs(Q_idx,1,:), record%specific_humidity, stack_qc=qc(Q_idx,1,:), stack_pc=pc(Q_idx,1,:), qc=record%specific_humidity_qc)
          record%specific_humidity_stack(:max_num_event) = prepbufr_stack(obs(Q_idx,1,:max_num_event))
          record%specific_humidity_stack_qc(:max_num_event) = prepbufr_codes(qc(Q_idx,1,:max_num_event))
          record%specific_humidity_stack_pc(:max_num_event) = prepbufr_codes(pc(Q_idx,1,:max_num_event))
        end if
        if (is_missing(record%dewpoint)) then
          call prepbufr_raw(obs(Td_idx,1,:), record%dewpoint)
        end if
        if (is_missing(record%wind_u)) then
          call prepbufr_raw(obs(u_idx,1,:), record%wind_u, stack_qc=qc(u_idx,1,:), stack_pc=pc(u_idx,1,:), qc=record%wind_qc)
          record%wind_u_stack(:max_num_event) = prepbufr_stack(obs(u_idx,1,:max_num_event))
          record%wind_stack_qc(:max_num_event) = prepbufr_codes(qc(u_idx,1,:max_num_event))
        end if
        if (is_missing(record%wind_v)) then
          call prepbufr_raw(obs(v_idx,1,:), record%wind_v, stack_qc=qc(v_idx,1,:), stack_pc=pc(v_idx,1,:), qc=record%wind_qc)
          record%wind_v_stack(:max_num_event) = prepbufr_stack(obs(v_idx,1,:max_num_event))
          record%wind_stack_pc(:max_num_event) = prepbufr_codes(pc(v_idx,1,:max_num_event))
        end if
        if (is_missing(record%wind_direction)) then
          call prepbufr_raw(obs(wd_idx,1,:), record%wind_direction, stack_qc=qc(wd_idx,1,:), stack_pc=pc(wd_idx,1,:), qc=record%wind_qc)
        end if
        if (is_missing(record%wind_speed)) then
          call prepbufr_raw(obs(ws_idx,1,:), record%wind_speed, stack_qc=qc(ws_idx,1,:), stack_pc=pc(ws_idx,1,:), qc=record%wind_qc)
        end if
        if (is_missing(record%rain_01h)) call prepbufr_raw(obs(TP01_idx,1,:), record%rain_01h)
        if (is_missing(record%rain_03h)) call prepbufr_raw(obs(TP03_idx,1,:), record%rain_03h)
        if (is_missing(record%rain_06h)) call prepbufr_raw(obs(TP06_idx,1,:), record%rain_06h)
        if (is_missing(record%rain_12h)) call prepbufr_raw(obs(TP12_idx,1,:), record%rain_12h)
        if (is_missing(record%rain_24h)) call prepbufr_raw(obs(TP24_idx,1,:), record%rain_24h)

        if (new_record) then
          call records%insert(station_name // '@' // time%isoformat(), record)
        ! else if (record%pressure_qc /= 2) then
        !   call debug_print(record, hdr, obs, qc, pc)
        end if
        call station%records%insert(trim(to_string(record%seq_id)), record, nodup=.true.)
      end do
    end do
    call closbf(10)

    write(*, *) '[Notice]: Station size is ' // trim(to_string(stations%size)) // ', record size is ' // trim(to_string(records%size)) // '.'

  end subroutine metar_prepbufr_read

  ! TODO: This subroutine is not completed.
  subroutine metar_prepbufr_write(file_path, stations, records)

    character(*), intent(inout) :: file_path
    type(hash_table_type), intent(in) :: stations
    type(linked_list_type), intent(in) :: records

    type(linked_list_iterator_type) record_iterator
    integer idate, iret
    real(8) hdr(max_num_var)
    real(8) obs(max_num_var,max_num_lev,max_num_event)
    real(8) qc(max_num_var,max_num_lev,max_num_event)
    real(8) pc(max_num_var,max_num_lev,max_num_event)

    if (file_path == '') file_path ='metar.prepbufr'

    hdr = 0.0
    obs = 0.0

    write(*, *) '[Notice]: Writing ' // trim(file_path) // ' ...'
    open(10, file=file_path, action='write', form='unformatted')
    open(11, file='../notes/prepobs_prep.bufrtable')
    call openbf(10, 'OUT', 11)
    record_iterator = linked_list_iterator(records)
    do while (.not. record_iterator%ended())
      select type (record => record_iterator%value)
      type is (metar_record_type)
        idate = record%time%year * 1000000 + record%time%month * 10000 + record%time%day * 100 + record%time%hour
        ! Mass part
        call openmb(10, 'ADPSFC', idate)
        hdr(1) = transfer(record%station%name, hdr(1))
        hdr(2) = record%station%lon
        hdr(3) = record%station%lat
        hdr(4) = record%station%z
        hdr(5) = 187
        hdr(6) = 0.0
        call ufbint(10, hdr, 6, 1, iret, 'SID XOB YOB ELV TYP DHR')
        obs(1,1,1) = divide(record%pressure, 100.0)
        obs(2,1,1) = record%temperature
        obs(3,1,1) = record%specific_humidity
        call ufbint(10, obs, 3, 1, iret, 'POB TOB QOB')
        qc(1,1,1) = record%pressure_qc
        qc(2,1,1) = record%temperature_qc
        qc(3,1,1) = record%specific_humidity_qc
        call ufbint(10, qc,  3, 1, iret, 'PQM TQM QQM')
        pc(1,1,1) = 1
        pc(2,1,1) = 1
        pc(3,1,1) = 1
        call ufbint(10, pc,  3, 1, iret, 'PPC TPC QPC')
        call writsb(10)
        call closmg(10)
        ! Wind part
        call openmb(10, 'ADPSFC', idate)
        hdr(1) = transfer(record%station%name, hdr(1))
        hdr(2) = record%station%lon
        hdr(3) = record%station%lat
        hdr(4) = record%station%z
        hdr(5) = 281
        hdr(6) = 0.0
        call ufbint(10, hdr, 6, 1, iret, 'SID XOB YOB ELV TYP DHR')
        obs(1,1,1) = divide(record%pressure, 100.0)
        obs(2,1,1) = record%wind_direction
        obs(3,1,1) = record%wind_speed
        obs(4,1,1) = record%wind_u
        obs(5,1,1) = record%wind_v
        call ufbint(10, obs, 5, 1, iret, 'POB DDO SOB UOB VOB')
        qc(1,1,1) = record%pressure_qc
        qc(2,1,1) = record%wind_qc
        call ufbint(10, qc,  2, 1, iret, 'PQM WQM')
        pc(1,1,1) = 1
        pc(2,1,1) = 1
        call ufbint(10, pc,  2, 1, iret, 'PPC WPC')
        call writsb(10)
        call closmg(10)
      end select
      call record_iterator%next()
    end do
    call closbf(10)

  end subroutine metar_prepbufr_write

end module metar_prepbufr_mod
