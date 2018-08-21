module amdar_prepbufr_mod

  use amdar_mod
  use datetime_mod
  use timedelta_mod
  use hash_table_mod
  use linked_list_mod
  use params_mod
  use utils_mod

  implicit none

  private

  public amdar_prepbufr_decode

  integer, parameter :: max_num_var = 35
  integer, parameter :: max_num_lev = 250
  integer, parameter :: max_num_event = 10

contains

  subroutine amdar_prepbufr_decode(file_path)

    character(*), intent(in) :: file_path

    character(8) subset, flight_name
    integer idate, iret, i
    character(10) sdate
    integer msg_count, subset_count
    real(8) hdr(max_num_var)
    real(8) obs(max_num_var,max_num_lev,max_num_event)
    real(8) qc(max_num_var,max_num_lev,max_num_event)
    real(8) pc(max_num_var,max_num_lev,max_num_event)
    real u, v
    type(datetime_type) time
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
      if (subset /= 'AIRCFT') cycle
      write(sdate, "(I10)") idate
      time = datetime(sdate, '%Y%m%d%H')
      write(*, "('=> ', I5.5, X, A8)") msg_count, subset
      do while (ireadsb(10) == 0) ! ireadsb copies one subset into internal arrays.
        ! Call values-level subrountines to retrieve actual data values from this subset.
        ! PRSLEVLA  RCT      ROLF        MSTQ        IALR        CAT        
        !           <P___INFO>  <Q___INFO>  <T___INFO>  <Z___INFO>
        !           <W___INFO>  <DRFTINFO>  [W1_EVENT]  <ACFT_SEQ>
        !           <TURB1SEQ>  <TURB2SEQ>  {TURB3SEQ}  {PREWXSEQ}
        !           {CLOUDSEQ}  {AFIC_SEQ}   NRLQMS
        call ufbint(10, hdr, max_num_var, 1,                          iret, 'SID DHR XOB YOB ELV TYP')
        !                                                                    1   2    3    4    5   6   7   8   9   10  11   12   13
        call ufbevn(10, obs, max_num_var, max_num_lev, max_num_event, iret, 'RCT ROLF MSTQ IALR CAT POB TOB QOB DDO FFO TDO TRBX')
        call ufbevn(10, qc,  max_num_var, max_num_lev, max_num_event, iret,                        'PQM TQM QQM DFQ NUL')
        call ufbevn(10, pc,  max_num_var, max_num_lev, max_num_event, iret,                        'PPC TPC QPC DFP NUL')
        flight_name = transfer(hdr(1), flight_name)
        time = time + timedelta(hours=int(hdr(2)))
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
        if (record%lon                     == real_missing_value) record%lon = hdr(3)
        if (record%lat                     == real_missing_value) record%lat = hdr(4)
        if (record%z                       == real_missing_value) record%z   = hdr(5)
        if (record%amdar_temperature       == real_missing_value) record%amdar_temperature       = prepbufr_raw(obs(7,1,:), pc(2,1,:))
        if (record%amdar_wind_speed        == real_missing_value) record%amdar_wind_speed        = prepbufr_raw(obs(10,1,:), pc(4,1,:))
        if (record%amdar_wind_direction    == real_missing_value) record%amdar_wind_direction    = prepbufr_raw(obs(9,1,:), pc(4,1,:))
        if (record%amdar_dewpoint          == real_missing_value) record%amdar_dewpoint          = prepbufr_raw(obs(11,1,:))
        if (record%amdar_specific_humidity == real_missing_value) record%amdar_specific_humidity = prepbufr_raw(obs(8,1,:), pc(3,1,:))

        if (new_record) then
          call records%insert(flight_name // '@' // time%isoformat(), record)
        end if
      end do
    end do
    call closbf(10)

  end subroutine amdar_prepbufr_decode

end module amdar_prepbufr_mod