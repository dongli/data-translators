module ship_prepbufr_mod

  use datetime
  use ship_mod
  use hash_table_mod
  use linked_list_mod
  use params_mod
  use utils_mod

  implicit none

  private

  public ship_prepbufr_read

  integer, parameter :: max_num_var = 20
  integer, parameter :: max_num_lev = 1
  integer, parameter :: max_num_event = 10

  integer, parameter :: p_idx    =  1
  integer, parameter :: T_idx    =  2
  integer, parameter :: Q_idx    =  3
  integer, parameter :: Td_idx   =  4
  integer, parameter :: u_idx    =  5
  integer, parameter :: v_idx    =  6
  integer, parameter :: SST_idx  =  7

contains

  ! Report types include: 180, 280, 282
  ! FIXME: Does 182 include SFCSHP?

  subroutine ship_prepbufr_read(file_path, ships, records)

    character(*), intent(in) :: file_path
    type(hash_table_type), intent(inout) :: ships
    type(linked_list_type), intent(inout) :: records

    character(8) subset, ship_name
    integer idate, iret, i
    character(10) sdate
    integer msg_count, subset_count
    real(8) hdr(max_num_var)
    real(8) obs(max_num_var,max_num_lev,max_num_event)
    real(8) qc(max_num_var,max_num_lev,max_num_event)
    real(8) pc(max_num_var,max_num_lev,max_num_event)
    type(datetime_type) base_time, time
    real lon, lat
    logical new_record
    type(ship_type), pointer :: ship
    type(ship_record_type), pointer :: record

    ! BUFRLIB functions
    integer ireadmg, ireadsb

    ships = hash_table(chunk_size=50000, max_load_factor=0.9)
    call records%clear()

    write(*, *) '[Notice]: Reading ' // trim(file_path) // ' ...'
    open(10, file=file_path, action='read', form='unformatted')
    call openbf(10, 'IN', 10)
    call datelen(10) ! This call causes idate to be in format YYYYMMDDHH.
    do while (ireadmg(10, subset, idate) == 0) ! ireadmg returns mnemonic in subset, and copies message into internal arrays.
      msg_count = msg_count + 1
      if (subset /= 'SFCSHP') cycle
      write(sdate, "(I10)") idate
      base_time = create_datetime(sdate, '%Y%m%d%H')
      ! write(*, "('=> ', I5.5, X, A8)") msg_count, subset
      do while (ireadsb(10) == 0) ! ireadsb copies one subset into internal arrays.
        ! Call values-level subrountines to retrieve actual data values from this subset.
        !                                                                    1   2   3   4   5   6   7   8
        call ufbint(10, hdr, max_num_var, 1,                          iret, 'SID XOB YOB ELV TYP DHR RPT TCOR')
        !                                                                    1   2   3   4   5   6   7     8   9    10   11
        call ufbevn(10, obs, max_num_var, max_num_lev, max_num_event, iret, 'POB TOB QOB TDO UOB VOB SST1  CAT ZOB')
        call ufbevn(10, qc,  max_num_var, max_num_lev, max_num_event, iret, 'PQM TQM QQM NUL WQM NUL SSTQM NUL ZQM')
        call ufbevn(10, pc,  max_num_var, max_num_lev, max_num_event, iret, 'PPC TPC QPC NUL WPC NUL SSTPC NUL ZPC')
        ship_name = transfer(hdr(1), ship_name)
        if (.not. (hdr(5) == 180 .or. hdr(5) == 183 .or. hdr(5) == 280 .or. hdr(5) == 282 .or. hdr(5) == 284)) then
          cycle
        end if
        time = base_time + timedelta(hours=hdr(6))
        lon = hdr(2)
        lat = hdr(3)
        if (ships%hashed(ship_name)) then
          select type (value => ships%value(ship_name))
          type is (ship_type)
            ship => value
          end select
        else
          allocate(ship)
          call ship%init(ship_name)
          call ships%insert(ship_name, ship)
        end if
        nullify(record)
        select type (value => records%last_value())
        type is (ship_record_type)
          ! Since record may be split into two subsets, we need to check if previous record exists with the same time.
          record => value
          if (record%ship%name == ship_name .and. record%time == time .and. &
            record%lon == lon .and. record%lat == lat) then
            new_record = .false.
          else
            nullify(record)
          end if
        end select
        if (.not. associated(record)) then
          allocate(record)
          record%seq_id = records%size
          record%ship => ship
          record%time = time
          record%lon = lon
          record%lat = lat
          new_record = .true.
        end if

        if (is_missing(record%ship_pressure)) then
          call prepbufr_raw(obs(p_idx,1,:), record%ship_pressure, stack_qc=qc(p_idx,1,:), stack_pc=pc(p_idx,1,:), qc=record%ship_pressure_qc)
          ! Convert pressure from hPa to Pa.
          record%ship_pressure = multiply(record%ship_pressure, 100.0)
        end if
        if (is_missing(record%ship_air_temperature)) then
          call prepbufr_raw(obs(T_idx,1,:), record%ship_air_temperature, stack_qc=qc(T_idx,1,:), stack_pc=pc(T_idx,1,:), qc=record%ship_air_temperature_qc)
        end if
        if (is_missing(record%ship_specific_humidity)) then
          call prepbufr_raw(obs(Q_idx,1,:), record%ship_specific_humidity, stack_qc=qc(Q_idx,1,:), stack_pc=pc(Q_idx,1,:), qc=record%ship_specific_humidity_qc)
        end if
        if (is_missing(record%ship_dewpoint)) then
          call prepbufr_raw(obs(Td_idx,1,:), record%ship_dewpoint)
        end if
        if (is_missing(record%ship_dewpoint)) then
          record%ship_dewpoint = dewpoint(record%ship_pressure, record%ship_specific_humidity)
        end if
        if (is_missing(record%ship_wind_speed)) then
          call prepbufr_raw(obs(u_idx,1,:), record%ship_wind_u, stack_qc=qc(u_idx,1,:), stack_pc=pc(u_idx,1,:), qc=record%ship_wind_qc)
          call prepbufr_raw(obs(v_idx,1,:), record%ship_wind_v, stack_qc=qc(u_idx,1,:), stack_pc=pc(u_idx,1,:), qc=record%ship_wind_qc)
          record%ship_wind_speed     = merge(real_missing_value, sqrt(record%ship_wind_u**2 + record%ship_wind_v**2), is_missing(record%ship_wind_u))
          record%ship_wind_direction = merge(real_missing_value, wind_direction(record%ship_wind_u, record%ship_wind_v), is_missing(record%ship_wind_u))
        end if
        if (is_missing(record%ship_sea_temperature)) then
          call prepbufr_raw(obs(SST_idx,1,:), record%ship_sea_temperature, stack_qc=qc(SST_idx,1,:), stack_pc=pc(SST_idx,1,:), qc=record%ship_sea_temperature_qc)
        end if

        if (new_record) then
          call records%insert(ship_name // '@' // time%isoformat(), record)
        ! else
        !   call debug_print(record, hdr, obs, qc, pc)
        end if
        call ship%records%insert(trim(to_string(record%seq_id)), record, nodup=.true.)
      end do
    end do
    call closbf(10)

    if (records%size == 0) then
      write(*, *) '[Warning]: There is no SHIP data!'
    else
      write(*, *) '[Notice]: Ship size is ' // trim(to_string(ships%size)) // ', record size is ' // trim(to_string(records%size)) // '.'
    end if

  end subroutine ship_prepbufr_read

  subroutine debug_print(record, hdr, obs, qc, pc)

    type(ship_record_type), intent(in) :: record
    real(8), intent(in) :: hdr(max_num_var)
    real(8), intent(in) :: obs(max_num_var,max_num_lev,max_num_event)
    real(8), intent(in) :: qc(max_num_var,max_num_lev,max_num_event)
    real(8), intent(in) :: pc(max_num_var,max_num_lev,max_num_event)

    print *, '--'
    print *, record%ship%name, record%time%isoformat(), hdr(5), hdr(6), hdr(7)
    print *, record%lon, record%lat
    print *, 'P  ', record%ship_pressure, record%ship_pressure_qc
    print *, 'TA ', record%ship_air_temperature, record%ship_air_temperature_qc
    print *, 'SST', record%ship_sea_temperature, record%ship_sea_temperature_qc
    print *, obs(SST_idx,1,:5)
    print *, 'RH ', record%ship_relative_humidity, record%ship_relative_humidity_qc
    print *, 'SH ', record%ship_specific_humidity, record%ship_specific_humidity_qc
    print *, obs(Q_idx,1,:5)
    print *, 'TD ', record%ship_dewpoint
    print *, 'U  ', record%ship_wind_u, record%ship_wind_qc
    print *, obs(u_idx,1,:5)
    print *, qc(u_idx,1,:5)
    print *, 'V  ', record%ship_wind_v, record%ship_wind_qc
    print *, obs(v_idx,1,:5)
    print *, 'CAT', obs(8,1,1)
    print *, 'Z  ', obs(9,1,1)

  end subroutine debug_print

end module ship_prepbufr_mod
