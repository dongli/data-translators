module amdar_prepbufr_mod

  use datetime
  use amdar_mod
  use hash_table_mod
  use linked_list_mod
  use params_mod
  use cli_mod
  use utils_mod

  implicit none

  private

  public amdar_prepbufr_read

  integer, parameter :: max_num_var = 20
  integer, parameter :: max_num_lev = 1
  integer, parameter :: max_num_event = 5

  integer, parameter :: p_idx    =  6
  integer, parameter :: z_idx    =  7
  integer, parameter :: T_idx    =  8
  integer, parameter :: Q_idx    =  9
  integer, parameter :: u_idx    = 10
  integer, parameter :: v_idx    = 11
  integer, parameter :: Td_idx   = 12
  integer, parameter :: trb_idx  = 13

contains

  subroutine amdar_prepbufr_read(file_path, flights, records)

    character(*), intent(in) :: file_path
    type(hash_table_type), intent(inout) :: flights
    type(linked_list_type), intent(inout) :: records

    character(8) subset, flight_name
    integer idate, iret, i
    character(10) sdate
    integer msg_count, subset_count
    real(8) hdr(max_num_var)
    real(8) obs(max_num_var,max_num_lev,max_num_event)
    real(8) qc(max_num_var,max_num_lev,max_num_event)
    real(8) pc(max_num_var,max_num_lev,max_num_event)
    type(datetime_type) base_time, time
    real lon, lat, p
    logical new_record
    type(amdar_flight_type), pointer :: flight
    type(amdar_record_type), pointer :: record

    ! BUFRLIB functions
    integer ireadmg, ireadsb

    flights = hash_table(chunk_size=50000, max_load_factor=0.9)
    call records%clear()

    write(*, *) '[Notice]: Reading ' // trim(file_path) // ' ...'
    open(10, file=file_path, action='read', form='unformatted')
    call openbf(10, 'IN', 10)
    call datelen(10) ! This call causes idate to be in format YYYYMMDDHH.
    do while (ireadmg(10, subset, idate) == 0) ! ireadmg returns mnemonic in subset, and copies message into internal arrays.
      msg_count = msg_count + 1
      ! Record AIRCFT and AIRCAR together.
      if (subset /= 'AIRCFT' .and. subset /= 'AIRCAR') cycle
      write(sdate, "(I10)") idate
      base_time = create_datetime(sdate, '%Y%m%d%H')
      do while (ireadsb(10) == 0) ! ireadsb copies one subset into internal arrays.
        ! Call values-level subrountines to retrieve actual data values from this subset.
        ! PRSLEVLA  RCT      ROLF        MSTQ        IALR        CAT        
        !           <P___INFO>  <Q___INFO>  <T___INFO>  <Z___INFO>
        !           <W___INFO>  <DRFTINFO>  [W1_EVENT]  <ACFT_SEQ>
        !           <TURB1SEQ>  <TURB2SEQ>  {TURB3SEQ}  {PREWXSEQ}
        !           {CLOUDSEQ}  {AFIC_SEQ}   NRLQMS
        !                                                                    1   2   3   4   5   6   7   8    9
        call ufbint(10, hdr, max_num_var, 1,                          iret, 'SID XOB YOB ELV TYP DHR RPT TCOR ACID')
        !                                                                    1   2    3    4    5   6   7   8   9   10  11  12  13
        call ufbevn(10, obs, max_num_var, max_num_lev, max_num_event, iret, 'RCT ROLF MSTQ IALR CAT POB ZOB TOB QOB UOB VOB TDO TRBX')
        call ufbevn(10, qc,  max_num_var, max_num_lev, max_num_event, iret, 'NUL NUL  NUL  NUL  NUL PQM ZQM TQM QQM WQM WQM NUL NUL')
        call ufbevn(10, pc,  max_num_var, max_num_lev, max_num_event, iret, 'NUL NUL  NUL  NUL  NUL PPC ZPC TPC QPC WPC WPC NUL NUL')
        flight_name = transfer(hdr(1), flight_name)
        lon = hdr(2)
        if (lon > 180) lon = lon - 360
        lat = hdr(3)
        call prepbufr_raw(obs(p_idx,1,:), p, stack_qc=qc(p_idx,1,:), stack_pc=pc(p_idx,1,:))
        p = multiply(p, 100.0)
        time = base_time + create_timedelta(hours=hdr(6))
        if (flights%hashed(flight_name)) then
          select type (value => flights%value(flight_name))
          type is (amdar_flight_type)
            flight => value
          end select
        else
          allocate(flight)
          call flight%init(flight_name)
          if (subset == 'AIRCAR') flight%number = transfer(hdr(9), flight%number)
          flight%seq_id = flights%size
          call flights%insert(flight_name, flight)
        end if
        nullify(record)
        select type (value => records%last_value())
        type is (amdar_record_type)
          ! Since record may be split into two subsets, we need to check if previous record exists with the same time.
          record => value
          if (record%flight%name /= flight_name .or. record%time /= time .or. record%lon /= lon .or. record%lat /= lat .or. record%p /= p) then
            nullify(record)
          else
            new_record = .false.
          end if
        end select
        if (.not. associated(record)) then
          allocate(record)
          record%seq_id = records%size
          select case (int(hdr(5)))
          case (130, 230)
            record%platform_type = 'AIREP'
          case (131, 231)
            record%platform_type = 'AMDAR'
          case (133, 233)
            record%platform_type = 'ACARS'
          end select
          record%flight => flight
          record%time = time
          new_record = .true.
        end if

        record%time = time
        if (is_missing(record%lon)) record%lon = lon
        if (is_missing(record%lat)) record%lat = lat
        if (is_missing(record%p)) then
          call prepbufr_raw(obs(p_idx,1,:), record%p, stack_qc=qc(p_idx,1,:), stack_pc=pc(p_idx,1,:), qc=record%p_qc)
          record%p_cr = prepbufr_correct(obs(p_idx,1,:), qc(p_idx,1,:), pc(p_idx,1,:))
        end if
        if (is_missing(record%h)) then
          call prepbufr_raw(obs(z_idx,1,:), record%h, stack_qc=qc(z_idx,1,:), stack_pc=pc(z_idx,1,:), qc=record%h_qc)
          record%h_cr = prepbufr_correct(obs(z_idx,1,:), qc(z_idx,1,:), pc(z_idx,1,:))
        end if
        if (is_missing(record%ta)) then
          call prepbufr_raw(obs(T_idx,1,:), record%ta, stack_qc=qc(T_idx,1,:), stack_pc=pc(T_idx,1,:), qc=record%ta_qc)
          record%ta_cr = prepbufr_correct(obs(T_idx,1,:), qc(T_idx,1,:), pc(T_idx,1,:))
        end if
        if (is_missing(record%sh)) then
          call prepbufr_raw(obs(Q_idx,1,:), record%sh, stack_qc=qc(Q_idx,1,:), stack_pc=pc(Q_idx,1,:), qc=record%sh_qc)
          record%sh_cr = prepbufr_correct(obs(Q_idx,1,:), qc(Q_idx,1,:), pc(Q_idx,1,:))
        end if
        if (is_missing(record%ws)) then
          call prepbufr_raw(obs(u_idx,1,:), record%ua, stack_qc=qc(u_idx,1,:), stack_pc=pc(u_idx,1,:), qc=record%ua_qc)
          call prepbufr_raw(obs(v_idx,1,:), record%va, stack_qc=qc(v_idx,1,:), stack_pc=pc(v_idx,1,:), qc=record%va_qc)
          record%ws = wind_speed(record%ua, record%va)
          record%wd = wind_direction(record%ua, record%va)
          record%ua_cr = prepbufr_correct(obs(u_idx,1,:), qc(u_idx,1,:), pc(u_idx,1,:))
          record%va_cr = prepbufr_correct(obs(v_idx,1,:), qc(v_idx,1,:), pc(v_idx,1,:))
        end if
        if (is_missing(record%td)) then
          call prepbufr_raw(obs(Td_idx,1,:), record%td)
        end if
        if (is_missing(record%td) .and. .not. is_missing(record%p) .and. .not. is_missing(record%sh)) then
          record%td = dewpoint(record%p, record%sh)
        end if
        record%rh = relative_humidity(record%p, record%ta, record%sh)
        if (record%trb == int_missing_value) then
          call prepbufr_raw(obs(trb_idx,1,:), record%trb)
        end if

        if (new_record) then
          call records%insert(flight_name // '@' // time%isoformat(), record)
        end if
        if (flight_name == cli_verbose_platform) then
          call record%print()
        end if
        call flight%records%insert(trim(to_string(record%seq_id)), record, nodup=.true.)
      end do
    end do
    call closbf(10)

    write(*, *) '[Notice]: Flight size is ' // trim(to_string(flights%size)) // ', record size is ' // trim(to_string(records%size)) // '.'

  end subroutine amdar_prepbufr_read

end module amdar_prepbufr_mod
