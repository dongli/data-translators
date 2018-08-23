module raob_prepbufr_mod

  use raob_mod
  use datetime_mod
  use timedelta_mod
  use hash_table_mod
  use linked_list_mod
  use params_mod
  use utils_mod

  implicit none

  private

  public raob_prepbufr_decode

  integer, parameter :: max_num_var = 35
  integer, parameter :: max_num_lev = 250
  integer, parameter :: max_num_event = 10

contains

  subroutine raob_prepbufr_decode(file_path)

    character(*), intent(in) :: file_path

    character(8) subset, station_name
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
    type(raob_station_type), pointer :: station
    type(raob_record_type), pointer :: record

    ! BUFRLIB functions
    integer ireadmg, ireadsb

    stations = hash_table(chunk_size=50000, max_load_factor=0.9)
    call records%clear()

    open(10, file=file_path, action='read', form='unformatted')
    call openbf(10, 'IN', 10)
    call datelen(10) ! This call causes idate to be in format YYYYMMDDHH.
    do while (ireadmg(10, subset, idate) == 0) ! ireadmg returns mnemonic in subset, and copies message into internal arrays.
      msg_count = msg_count + 1
      if (subset /= 'ADPUPA') cycle
      write(sdate, "(I10)") idate
      time = datetime(sdate, '%Y%m%d%H')
      write(*, "('=> ', I5.5, X, A8)") msg_count, subset
      do while (ireadsb(10) == 0) ! ireadsb copies one subset into internal arrays.
        ! Call values-level subrountines to retrieve actual data values from this subset.
        call ufbint(10, hdr, max_num_var, 1,                          iret, 'SID DHR XOB YOB ELV TYP')
        !                                                                    1   2    3    4    5   6   7   8   9   10  11   12   13
        call ufbevn(10, obs, max_num_var, max_num_lev, max_num_event, iret, 'RCT ROLF MSTQ IALR CAT POB TOB QOB DDO FFO TDO TRBX')
        call ufbevn(10, qc,  max_num_var, max_num_lev, max_num_event, iret,                        'PQM TQM QQM DFQ NUL')
        call ufbevn(10, pc,  max_num_var, max_num_lev, max_num_event, iret,                        'PPC TPC QPC DFP NUL')
        station_name = transfer(hdr(1), station_name)
        print *, station_name, hdr(3), hdr(4), hdr(5)
        cycle
        time = time + timedelta(hours=int(hdr(2)))
        if (stations%hashed(station_name)) then
          select type (value => stations%value(station_name))
          type is (raob_station_type)
            station => value
          end select
        else
          allocate(station)
          station%name = station_name
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
          record%station => station
          record%time = time
          new_record = .true.
        end if

        if (new_record) then
          call records%insert(station_name // '@' // time%isoformat(), record)
        end if
      end do
    end do
    call closbf(10)

  end subroutine raob_prepbufr_decode

end module