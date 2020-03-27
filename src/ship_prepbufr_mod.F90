module ship_prepbufr_mod

  use datetime
  use container
  use flogger
  use params_mod
  use utils_mod
  use ship_mod

  implicit none

  private

  public ship_prepbufr_read

  integer, parameter :: max_num_var = 20
  integer, parameter :: max_num_lev = 1
  integer, parameter :: max_num_event = 10

  integer, parameter :: p_idx    =  1
  integer, parameter :: ta_idx   =  2
  integer, parameter :: sh_idx   =  3
  integer, parameter :: td_idx   =  4
  integer, parameter :: ua_idx   =  5
  integer, parameter :: va_idx   =  6
  integer, parameter :: sst_idx  =  7

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

    call log_notice('Reading ' // trim(file_path) // ' ...')
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
        !                                                                    1   2   3   4   5   6   7     8
        call ufbint(10, hdr, max_num_var, 1,                          iret, 'SID XOB YOB ELV TYP DHR RPT   TCOR')
        call ufbevn(10, obs, max_num_var, max_num_lev, max_num_event, iret, 'POB TOB QOB TDO UOB VOB SST1  CAT ZOB')
        call ufbevn(10, qc,  max_num_var, max_num_lev, max_num_event, iret, 'PQM TQM QQM NUL WQM NUL SSTQM NUL ZQM')
        call ufbevn(10, pc,  max_num_var, max_num_lev, max_num_event, iret, 'PPC TPC QPC NUL WPC NUL SSTPC NUL ZPC')
        ship_name = transfer(hdr(1), ship_name)
        if (.not. (hdr(5) == 180 .or. hdr(5) == 183 .or. hdr(5) == 280 .or. hdr(5) == 282 .or. hdr(5) == 284)) then
          cycle
        end if
        time = base_time + create_timedelta(hours=hdr(6))
        lon = hdr(2)
        if (lon > 180) lon = lon - 360
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

        if (is_missing(record%p)) then
          call prepbufr_raw(obs(p_idx,1,:), record%p, stack_qc=qc(p_idx,1,:), stack_pc=pc(p_idx,1,:), qc=record%p_qc)
        end if
        if (is_missing(record%ta)) then
          call prepbufr_raw(obs(ta_idx,1,:), record%ta, stack_qc=qc(ta_idx,1,:), stack_pc=pc(ta_idx,1,:), qc=record%ta_qc)
        end if
        if (is_missing(record%sh)) then
          call prepbufr_raw(obs(sh_idx,1,:), record%sh, stack_qc=qc(sh_idx,1,:), stack_pc=pc(sh_idx,1,:), qc=record%sh_qc)
        end if
        if (is_missing(record%td)) then
          call prepbufr_raw(obs(td_idx,1,:), record%td)
        end if
        if (is_missing(record%td)) then
          record%td = dewpoint(record%p, record%sh)
        end if
        if (is_missing(record%ws)) then
          call prepbufr_raw(obs(ua_idx,1,:), record%ua, stack_qc=qc(ua_idx,1,:), stack_pc=pc(ua_idx,1,:), qc=record%ua_qc)
          call prepbufr_raw(obs(va_idx,1,:), record%va, stack_qc=qc(ua_idx,1,:), stack_pc=pc(ua_idx,1,:), qc=record%va_qc)
          record%ws = merge(real_missing_value, sqrt(record%ua**2 + record%va**2), is_missing(record%ua))
          record%wd = merge(real_missing_value, wind_direction(record%ua, record%va), is_missing(record%ua))
        end if
        if (is_missing(record%sst)) then
          call prepbufr_raw(obs(sst_idx,1,:), record%sst, stack_qc=qc(sst_idx,1,:), stack_pc=pc(sst_idx,1,:), qc=record%sst_qc)
        end if

        if (new_record) then
          call records%insert(ship_name // '@' // time%isoformat(), record)
        ! else
        !   call record%print()
        end if
        call ship%records%insert(trim(to_string(record%seq_id)), record, nodup=.true.)
      end do
    end do
    call closbf(10)

    if (records%size == 0) then
      call log_warning('There is no SHIP data!')
    else
      call log_notice('Ship size is ' // trim(to_string(ships%size)) // ', record size is ' // trim(to_string(records%size)) // '.')
    end if

  end subroutine ship_prepbufr_read

end module ship_prepbufr_mod
