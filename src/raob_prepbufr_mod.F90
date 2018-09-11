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
    character(20) key
    integer msg_count, subset_count, num_level
    real(8) hdr(max_num_var)
    real(8) obs(max_num_var,max_num_lev,max_num_event)
    real(8) qc(max_num_var,max_num_lev,max_num_event)
    real(8) pc(max_num_var,max_num_lev,max_num_event)
    real u, v, value
    type(datetime_type) time
    logical new_record
    type(raob_station_type), pointer :: station
    type(raob_decode_record_type), pointer :: record

    ! BUFRLIB functions
    integer ireadmg, ireadsb

    stations = hash_table(chunk_size=50000, max_load_factor=0.9)
    call records%clear()
    nullify(record)

    open(10, file=file_path, action='read', form='unformatted')
    call openbf(10, 'IN', 10)
    call datelen(10) ! This call causes idate to be in format YYYYMMDDHH.
    do while (ireadmg(10, subset, idate) == 0) ! ireadmg returns mnemonic in subset, and copies message into internal arrays.
      msg_count = msg_count + 1
      if (subset /= 'ADPUPA') cycle
      write(sdate, "(I10)") idate
      time = create_datetime(sdate, '%Y%m%d%H')
      write(*, "('=> ', I5.5, X, A8)") msg_count, subset
      do while (ireadsb(10) == 0) ! ireadsb copies one subset into internal arrays.
        ! Call values-level subrountines to retrieve actual data values from this subset.
        !                                                                    1   2   3   4   5   6   7   8
        call ufbint(10, hdr, max_num_var, 1,                          iret, 'SID XOB YOB ELV TYP DHR RPT TCOR')
        !                                                                    1   2   3   4   5   6   7
        call ufbevn(10, obs, max_num_var, max_num_lev, max_num_event, iret, 'CAT POB TOB QOB TDO DDO FFO')
        call ufbevn(10, qc,  max_num_var, max_num_lev, max_num_event, iret, 'NUL PQM TQM QQM NUL DFQ NUL')
        call ufbevn(10, pc,  max_num_var, max_num_lev, max_num_event, iret, 'NUL PPC TPC QPC NUL DFP NUL')
        station_name = transfer(hdr(1), station_name)
        ! Filter out non-RAOB observations.
        if (.not. (hdr(5) == 120 .or. hdr(5) == 220) .or. len_trim(station_name) /= 5) cycle
        time = time + timedelta(hours=int(hdr(6)))
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
        type is (raob_decode_record_type)
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
          call record%init()
          record%station => station
          record%time = time
          new_record = .true.
        end if

        num_level = prepbufr_value_count(obs(1,:,1))
        do i = 1, num_level
          key = to_string(prepbufr_raw(obs(2,i,:), qc(2,i,:), pc(2,i,:)))
          select case (int(obs(1,i,1)))
          case (1)
            value = prepbufr_raw(obs(2,i,:), qc(2,i,:), pc(2,i,:))
            if (.not. record%snd_man_pressure%hashed(key) .or. value /= real_missing_value) then
              call record%snd_man_pressure%insert(key, value)
            end if
            value = prepbufr_raw(obs(3,i,:), qc(3,i,:), pc(3,i,:))
            if (.not. record%snd_man_temperature%hashed(key) .or. value /= real_missing_value) then
              call record%snd_man_temperature%insert(key, value)
            end if
            value = prepbufr_raw(obs(4,i,:), qc(4,i,:), pc(4,i,:))
            if (.not. record%snd_man_specific_humidity%hashed(key) .or. value /= real_missing_value) then
              call record%snd_man_specific_humidity%insert(key, value)
            end if
            value = prepbufr_raw(obs(5,i,:), qc(5,i,:), pc(5,i,:))
            if (.not. record%snd_man_dewpoint%hashed(key) .or. value /= real_missing_value) then
              call record%snd_man_dewpoint%insert(key, value)
            end if
            value = prepbufr_raw(obs(6,i,:), qc(6,i,:), pc(6,i,:))
            if (.not. record%snd_man_wind_direction%hashed(key) .or. value /= real_missing_value) then
              call record%snd_man_wind_direction%insert(key, value)
            end if
            value = prepbufr_raw(obs(7,i,:), qc(7,i,:), pc(7,i,:))
            if (.not. record%snd_man_wind_speed%hashed(key) .or. value /= real_missing_value) then
              call record%snd_man_wind_speed%insert(key, value)
            end if
          case (2)
            value = prepbufr_raw(obs(2,i,:), qc(2,i,:), pc(2,i,:))
            if (.not. record%snd_sig_pressure%hashed(key) .or. value /= real_missing_value) then
              call record%snd_sig_pressure%insert(key, value)
            end if
            value = prepbufr_raw(obs(3,i,:), qc(3,i,:), pc(3,i,:))
            if (.not. record%snd_sig_temperature%hashed(key) .or. value /= real_missing_value) then
              call record%snd_sig_temperature%insert(key, value)
            end if
            value = prepbufr_raw(obs(4,i,:), qc(4,i,:), pc(4,i,:))
            if (.not. record%snd_sig_specific_humidity%hashed(key) .or. value /= real_missing_value) then
              call record%snd_sig_specific_humidity%insert(key, value)
            end if
            value = prepbufr_raw(obs(5,i,:), qc(5,i,:), pc(5,i,:))
            if (.not. record%snd_sig_dewpoint%hashed(key) .or. value /= real_missing_value) then
              call record%snd_sig_dewpoint%insert(key, value)
            end if
            value = prepbufr_raw(obs(6,i,:), qc(6,i,:), pc(6,i,:))
            if (.not. record%snd_sig_wind_direction%hashed(key) .or. value /= real_missing_value) then
              call record%snd_sig_wind_direction%insert(key, value)
            end if
            value = prepbufr_raw(obs(7,i,:), qc(7,i,:), pc(7,i,:))
            if (.not. record%snd_sig_wind_speed%hashed(key) .or. value /= real_missing_value) then
              call record%snd_sig_wind_speed%insert(key, value)
            end if
          case (3, 4)
            value = prepbufr_raw(obs(2,i,:), qc(2,i,:), pc(2,i,:))
            if (.not. record%snd_wnd_pressure%hashed(key) .or. value /= real_missing_value) then
              call record%snd_wnd_pressure%insert(key, value)
            end if
            value = prepbufr_raw(obs(6,i,:), qc(6,i,:), pc(6,i,:))
            if (.not. record%snd_wnd_wind_direction%hashed(key) .or. value /= real_missing_value) then
              call record%snd_wnd_wind_direction%insert(key, value)
            end if
            value = prepbufr_raw(obs(7,i,:), qc(7,i,:), pc(7,i,:))
            if (.not. record%snd_wnd_wind_speed%hashed(key) .or. value /= real_missing_value) then
              call record%snd_wnd_wind_speed%insert(key, value)
            end if
          case (5)
            value = prepbufr_raw(obs(2,i,:), qc(2,i,:), pc(2,i,:))
            if (.not. record%snd_trop_pressure%hashed(key) .or. value /= real_missing_value) then
              call record%snd_trop_pressure%insert(key, value)
            end if
            value = prepbufr_raw(obs(3,i,:), qc(3,i,:), pc(3,i,:))
            if (.not. record%snd_trop_temperature%hashed(key) .or. value /= real_missing_value) then
              call record%snd_trop_temperature%insert(key, value)
            end if
            value = prepbufr_raw(obs(4,i,:), qc(4,i,:), pc(4,i,:))
            if (.not. record%snd_trop_specific_humidity%hashed(key) .or. value /= real_missing_value) then
              call record%snd_trop_specific_humidity%insert(key, value)
            end if
            value = prepbufr_raw(obs(5,i,:), qc(5,i,:), pc(5,i,:))
            if (.not. record%snd_trop_dewpoint%hashed(key) .or. value /= real_missing_value) then
              call record%snd_trop_dewpoint%insert(key, value)
            end if
            value = prepbufr_raw(obs(6,i,:), qc(6,i,:), pc(6,i,:))
            if (.not. record%snd_trop_wind_direction%hashed(key) .or. value /= real_missing_value) then
              call record%snd_trop_wind_direction%insert(key, value)
            end if
            value = prepbufr_raw(obs(7,i,:), qc(7,i,:), pc(7,i,:))
            if (.not. record%snd_trop_wind_speed%hashed(key) .or. value /= real_missing_value) then
              call record%snd_trop_wind_speed%insert(key, value)
            end if
          end select
        end do

        if (new_record) then
          call records%insert(station_name // '@' // time%isoformat(), record)
        else
          call debug_print(record, obs, qc, pc)
        end if
      end do
    end do
    call closbf(10)

  end subroutine raob_prepbufr_decode

  subroutine debug_print(record, obs, qc, pc)

    type(raob_decode_record_type), intent(in) :: record
    real(8), intent(in) :: obs(max_num_var,max_num_lev,max_num_event)
    real(8), intent(in) :: qc(max_num_var,max_num_lev,max_num_event)
    real(8), intent(in) :: pc(max_num_var,max_num_lev,max_num_event)

    integer num_level, i
    type(hash_table_iterator_type) record_iterator

    print *, 'Station ', record%station%name
    print *, '- Mandatory levels:'
    write(*, '(6A15)') 'P', 'T', 'SH', 'TD', 'WD', 'WS'
    record_iterator = hash_table_iterator(record%snd_man_pressure)
    do while (.not. record_iterator%ended())
      select type (value => record_iterator%value)
      type is (real)
        write(*, '(F15.1)', advance='no') value
      end select
      select type (value => record%snd_man_temperature%value(record_iterator%key))
      type is (real)
        write(*, '(F15.1)', advance='no') value
      end select
      select type (value => record%snd_man_specific_humidity%value(record_iterator%key))
      type is (real)
        write(*, '(F15.1)', advance='no') value
      end select
      select type (value => record%snd_man_dewpoint%value(record_iterator%key))
      type is (real)
        write(*, '(F15.1)', advance='no') value
      end select
      select type (value => record%snd_man_wind_direction%value(record_iterator%key))
      type is (real)
        write(*, '(F15.1)', advance='no') value
      end select
      select type (value => record%snd_man_wind_speed%value(record_iterator%key))
      type is (real)
        write(*, '(F15.1)', advance='no') value
      end select
      write(*, *)
      call record_iterator%next()
    end do
    print *, '- Significant levels:'
    write(*, '(6A15)') 'P', 'T', 'SH', 'TD', 'WD', 'WS'
    record_iterator = hash_table_iterator(record%snd_sig_pressure)
    do while (.not. record_iterator%ended())
      select type (value => record_iterator%value)
      type is (real)
        write(*, '(F15.1)', advance='no') value
      end select
      select type (value => record%snd_sig_temperature%value(record_iterator%key))
      type is (real)
        write(*, '(F15.1)', advance='no') value
      end select
      select type (value => record%snd_sig_specific_humidity%value(record_iterator%key))
      type is (real)
        write(*, '(F15.1)', advance='no') value
      end select
      select type (value => record%snd_sig_dewpoint%value(record_iterator%key))
      type is (real)
        write(*, '(F15.1)', advance='no') value
      end select
      select type (value => record%snd_sig_wind_direction%value(record_iterator%key))
      type is (real)
        write(*, '(F15.1)', advance='no') value
      end select
      select type (value => record%snd_sig_wind_speed%value(record_iterator%key))
      type is (real)
        write(*, '(F15.1)', advance='no') value
      end select
      write(*, *)
      call record_iterator%next()
    end do
    print *, '- Wind levels:'
    write(*, '(A15, 45X, 2A15)') 'P', 'WD', 'WS'
    record_iterator = hash_table_iterator(record%snd_wnd_pressure)
    do while (.not. record_iterator%ended())
      select type (value => record_iterator%value)
      type is (real)
        write(*, '(F15.1)', advance='no') value
      end select
      write(*, '(45X)', advance='no')
      select type (value => record%snd_wnd_wind_direction%value(record_iterator%key))
      type is (real)
        write(*, '(F15.1)', advance='no') value
      end select
      select type (value => record%snd_wnd_wind_speed%value(record_iterator%key))
      type is (real)
        write(*, '(F15.1)', advance='no') value
      end select
      write(*, *)
      call record_iterator%next()
    end do
    print *, '- Tropopause levels:'
    write(*, '(6A15)') 'P', 'T', 'SH', 'TD', 'WD', 'WS'
    record_iterator = hash_table_iterator(record%snd_trop_pressure)
    do while (.not. record_iterator%ended())
      select type (value => record_iterator%value)
      type is (real)
        write(*, '(F15.1)', advance='no') value
      end select
      select type (value => record%snd_trop_temperature%value(record_iterator%key))
      type is (real)
        write(*, '(F15.1)', advance='no') value
      end select
      select type (value => record%snd_trop_specific_humidity%value(record_iterator%key))
      type is (real)
        write(*, '(F15.1)', advance='no') value
      end select
      select type (value => record%snd_trop_dewpoint%value(record_iterator%key))
      type is (real)
        write(*, '(F15.1)', advance='no') value
      end select
      select type (value => record%snd_trop_wind_direction%value(record_iterator%key))
      type is (real)
        write(*, '(F15.1)', advance='no') value
      end select
      select type (value => record%snd_trop_wind_speed%value(record_iterator%key))
      type is (real)
        write(*, '(F15.1)', advance='no') value
      end select
      write(*, *)
      call record_iterator%next()
    end do

    ! num_level = prepbufr_value_count(obs(1,:,1))
    ! write(*, '(' // to_string(num_level) // 'I2)') int(obs(1,:num_level,1))
    ! do i = 1, num_level
    !   write(*, '(I2, 6F15.1)') int(obs(1,i,1)), obs(2,i,1), obs(3,i,1), obs(4,i,1), obs(5,i,1), obs(6,i,1), obs(7,i,1)
    ! end do
    ! print *, count(obs(1,:,1) == 1)

  end subroutine debug_print

end module
