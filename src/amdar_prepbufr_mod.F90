module amdar_prepbufr_mod

  use amdar_mod
  use datetime
  use hash_table_mod
  use linked_list_mod
  use params_mod
  use utils_mod

  implicit none

  private

  public amdar_prepbufr_read

  integer, parameter :: max_num_var = 20
  integer, parameter :: max_num_lev = 1
  integer, parameter :: max_num_event = 5

contains

  subroutine amdar_prepbufr_read(file_path)

    character(*), intent(in) :: file_path

    character(8) subset, flight_name
    integer idate, iret, i
    character(10) sdate
    integer msg_count, subset_count
    real(8) hdr(max_num_var)
    real(8) obs(max_num_var,max_num_lev,max_num_event)
    real(8) qc(max_num_var,max_num_lev,max_num_event)
    real(8) pc(max_num_var,max_num_lev,max_num_event)
    type(datetime_type) base_time, time
    logical new_record
    type(amdar_flight_type), pointer :: flight
    type(amdar_record_type), pointer :: record

    ! BUFRLIB functions
    integer ireadmg, ireadsb

    flights = hash_table(chunk_size=50000, max_load_factor=0.9)
    call records%clear()

    open(10, file=file_path, action='read', form='unformatted')
    call openbf(10, 'IN', 10)
    call datelen(10) ! This call causes idate to be in format YYYYMMDDHH.
    do while (ireadmg(10, subset, idate) == 0) ! ireadmg returns mnemonic in subset, and copies message into internal arrays.
      msg_count = msg_count + 1
      if (subset /= 'AIRCFT' .and. subset /= 'AIRCAR') cycle
      write(sdate, "(I10)") idate
      base_time = create_datetime(sdate, '%Y%m%d%H')
      write(*, "('=> ', I5.5, X, A8)") msg_count, subset
      do while (ireadsb(10) == 0) ! ireadsb copies one subset into internal arrays.
        ! Call values-level subrountines to retrieve actual data values from this subset.
        ! PRSLEVLA  RCT      ROLF        MSTQ        IALR        CAT        
        !           <P___INFO>  <Q___INFO>  <T___INFO>  <Z___INFO>
        !           <W___INFO>  <DRFTINFO>  [W1_EVENT]  <ACFT_SEQ>
        !           <TURB1SEQ>  <TURB2SEQ>  {TURB3SEQ}  {PREWXSEQ}
        !           {CLOUDSEQ}  {AFIC_SEQ}   NRLQMS
        !                                                                    1   2   3   4   5   6   7   8
        call ufbint(10, hdr, max_num_var, 1,                          iret, 'SID XOB YOB ELV TYP DHR RPT TCOR')
        !                                                                    1   2    3    4    5   6   7   8   9   10  11  12
        call ufbevn(10, obs, max_num_var, max_num_lev, max_num_event, iret, 'RCT ROLF MSTQ IALR CAT POB TOB QOB UOB VOB TDO TRBX')
        call ufbevn(10, qc,  max_num_var, max_num_lev, max_num_event, iret,                        'PQM TQM QQM WQM NUL')
        call ufbevn(10, pc,  max_num_var, max_num_lev, max_num_event, iret,                        'PPC TPC QPC WPC NUL')
        flight_name = transfer(hdr(1), flight_name)
        time = base_time + timedelta(hours=int(hdr(6)))
        if (flights%hashed(flight_name)) then
          select type (value => flights%value(flight_name))
          type is (amdar_flight_type)
            flight => value
          end select
        else
          allocate(flight)
          flight%name = flight_name
          call flights%insert(flight_name, flight)
        end if
        nullify(record)
        select type (value => records%last_value())
        type is (amdar_record_type)
          ! Since recode may be split into two subsets, we need to check if previous record exists with the same time.
          record => value
          if (record%flight%name == flight_name .and. record%time == time) then
            new_record = .false.
          else
            nullify(record)
          end if
        end select
        if (.not. associated(record)) then
          allocate(record)
          record%flight => flight
          record%time = time
          new_record = .true.
        end if

        record%time = time
        if (record%lon                     == real_missing_value) record%lon = hdr(2)
        if (record%lat                     == real_missing_value) record%lat = hdr(3)
        if (record%z                       == real_missing_value) record%z   = hdr(4)
        if (record%amdar_pressure == real_missing_value) then
          call prepbufr_raw(obs(6,1,:), record%amdar_pressure, stack_qc=qc(1,1,:), stack_pc=pc(1,1,:), qc=record%amdar_pressure_qc)
        end if
        if (record%amdar_temperature == real_missing_value) then
          call prepbufr_raw(obs(7,1,:), record%amdar_temperature, stack_qc=qc(2,1,:), stack_pc=pc(2,1,:), qc=record%amdar_temperature_qc)
        end if
        if (record%amdar_wind_speed == real_missing_value) then
          call prepbufr_raw(obs( 9,1,:), record%amdar_wind_u, stack_qc=qc(4,1,:), stack_pc=pc(4,1,:), qc=record%amdar_wind_qc)
          call prepbufr_raw(obs(10,1,:), record%amdar_wind_v, stack_qc=qc(4,1,:), stack_pc=pc(4,1,:), qc=record%amdar_wind_qc)
          record%amdar_wind_speed     = merge(real_missing_value, sqrt(record%amdar_wind_u**2 + record%amdar_wind_v**2), record%amdar_wind_u == real_missing_value)
          record%amdar_wind_direction = merge(real_missing_value, wind_direction(record%amdar_wind_u, record%amdar_wind_v), record%amdar_wind_u == real_missing_value)
        end if
        if (record%amdar_dewpoint == real_missing_value) then
          call prepbufr_raw(obs(11,1,:), record%amdar_dewpoint)
        end if
        if (record%amdar_specific_humidity == real_missing_value) then
          call prepbufr_raw(obs(8,1,:), record%amdar_specific_humidity, stack_qc=qc(3,1,:), stack_pc=pc(3,1,:), qc=record%amdar_specific_humidity_qc)
        end if
        if (record%amdar_turbulence_index == int_missing_value) then
          call prepbufr_raw(obs(12,1,:), record%amdar_turbulence_index)
        end if

        if (new_record) then
          call records%insert(flight_name // '@' // time%isoformat(), record)
        ! else
        !   if (flight_name == '554VVRBA') call debug_print(record, hdr, obs, qc, pc)
        end if
      end do
    end do
    call closbf(10)

  end subroutine amdar_prepbufr_read

  subroutine debug_print(record, hdr, obs, qc, pc)

    type(amdar_record_type), intent(in) :: record
    real(8), intent(in) :: hdr(max_num_var)
    real(8), intent(in) :: obs(max_num_var,max_num_lev,max_num_event)
    real(8), intent(in) :: qc(max_num_var,max_num_lev,max_num_event)
    real(8), intent(in) :: pc(max_num_var,max_num_lev,max_num_event)

    print *, '--'
    print *, record%flight%name, record%time%isoformat(), hdr(6), hdr(7)
    print *, record%lon, record%lat, record%z
    print *, 'T ', record%amdar_temperature
    print *, 'T ', obs(7,1,:)
    print *, 'T ', qc(2,1,:)
    print *, 'T ', pc(2,1,:)
    print *, 'P ', record%amdar_pressure
    print *, 'P ', obs(6,1,:)
    print *, 'P ', qc(1,1,:)
    print *, 'P ', pc(1,1,:)
    print *, 'W ', record%amdar_wind_u, record%amdar_wind_v
    print *, 'TRBX ', record%amdar_turbulence_index

  end subroutine debug_print

end module amdar_prepbufr_mod
