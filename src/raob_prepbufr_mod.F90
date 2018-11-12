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

  public raob_prepbufr_read

  integer, parameter :: max_num_var = 20
  integer, parameter :: max_num_lev = 100
  integer, parameter :: max_num_event = 10

contains

  subroutine raob_prepbufr_read(file_path, stations, records)

    character(*), intent(in) :: file_path
    type(hash_table_type), intent(inout) :: stations
    type(linked_list_type), intent(inout) :: records

    character(8) subset, station_name
    integer idate, iret, i
    character(10) sdate
    character(20) key
    integer msg_count, subset_count, num_level
    real(8) hdr(max_num_var)
    real(8) obs(max_num_var,max_num_lev,max_num_event)
    real(8) qc(max_num_var,max_num_lev,max_num_event)
    real(8) pc(max_num_var,max_num_lev,max_num_event)
    real p, T, sh, Td, rh, u, v, h
    integer p_qc, T_qc, sh_qc, uv_qc
    type(datetime_type) base_time, time
    logical new_record
    type(raob_station_type), pointer :: station
    type(raob_read_record_type), pointer :: record

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
      base_time = create_datetime(sdate, '%Y%m%d%H')
      write(*, "('=> ', I5.5, X, A8)") msg_count, subset
      do while (ireadsb(10) == 0) ! ireadsb copies one subset into internal arrays.
        ! Call values-level subrountines to retrieve actual data values from this subset.
        !                                                                    1   2   3   4   5   6   7   8
        call ufbint(10, hdr, max_num_var, 1,                          iret, 'SID XOB YOB ELV TYP DHR RPT TCOR')
        !                                                                    1   2   3   4   5   6   7   8
        call ufbevn(10, obs, max_num_var, max_num_lev, max_num_event, iret, 'CAT POB TOB QOB TDO UOB VOB ZOB')
        call ufbevn(10, qc,  max_num_var, max_num_lev, max_num_event, iret, 'NUL PQM TQM QQM NUL WQM NUL ZQM')
        call ufbevn(10, pc,  max_num_var, max_num_lev, max_num_event, iret, 'NUL PPC TPC QPC NUL WPC NUL ZPC')
        station_name = transfer(hdr(1), station_name)
        ! Filter out non-RAOB observations.
        if (.not. (hdr(5) == 120 .or. hdr(5) == 220) .or. len_trim(station_name) /= 5) cycle
        time = base_time + timedelta(hours=hdr(6))
        if (stations%hashed(station_name)) then
          select type (value => stations%value(station_name))
          type is (raob_station_type)
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
        type is (raob_read_record_type)
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
          call prepbufr_raw(obs(2,i,:), p, stack_qc=qc(2,i,:), stack_pc=pc(2,i,:), qc=p_qc)
          if (is_missing(p)) cycle
          p = p * 100 ! Convert units from hPa to Pa.
          key = to_string(p)
          select case (int(obs(1,i,1)))
          case (1) ! Mandatory level
            if (.not. record%snd_man_pressure%hashed(key) .and. .not. is_missing(p)) then
              call record%snd_man_pressure%insert(key, p)
              call record%snd_man_pressure_qc%insert(key, p_qc)
            end if
            call prepbufr_raw(obs(3,i,:), T, stack_qc=qc(3,i,:), stack_pc=pc(3,i,:), qc=T_qc)
            if (.not. record%snd_man_temperature%hashed(key) .and. .not. is_missing(T)) then
              call record%snd_man_temperature%insert(key, T)
              call record%snd_man_temperature_qc%insert(key, T_qc)
            end if
            call prepbufr_raw(obs(4,i,:), sh, stack_qc=qc(4,i,:), stack_pc=pc(4,i,:), qc=sh_qc)
            if (.not. record%snd_man_specific_humidity%hashed(key) .and. .not. is_missing(sh)) then
              call record%snd_man_specific_humidity%insert(key, sh)
              call record%snd_man_humidity_qc%insert(key, sh_qc)
            end if
            call prepbufr_raw(obs(5,i,:), Td, stack_qc=qc(5,i,:), stack_pc=pc(5,i,:))
            if (.not. record%snd_man_dewpoint%hashed(key)) then
              if (is_missing(Td)) Td = dewpoint(p, sh)
              if (.not. is_missing(Td)) call record%snd_man_dewpoint%insert(key, Td)
            end if
            rh = relative_humidity(p, T, sh)
            if (.not. record%snd_man_relative_humidity%hashed(key) .and. .not. is_missing(rh)) then
              call record%snd_man_relative_humidity%insert(key, rh)
            end if
            call prepbufr_raw(obs(6,i,:), u, stack_qc=qc(6,i,:), stack_pc=pc(6,i,:), qc=uv_qc)
            call prepbufr_raw(obs(7,i,:), v, stack_qc=qc(6,i,:), stack_pc=pc(6,i,:))
            if (.not. record%snd_man_wind_u%hashed(key) .and. .not. is_missing(u) .and. .not. is_missing(v)) then
              call record%snd_man_wind_u%insert(key, u)
              call record%snd_man_wind_v%insert(key, v)
              call record%snd_man_wind_speed%insert(key, sqrt(u**2 + v**2))
              call record%snd_man_wind_direction%insert(key, wind_direction(u, v))
              call record%snd_man_wind_qc%insert(key, uv_qc)
            end if
            call prepbufr_raw(obs(8,i,:), h, stack_qc=qc(8,i,:), stack_pc=pc(8,i,:))
            if (.not. record%snd_man_height%hashed(key) .and. .not. is_missing(h)) then
              call record%snd_man_height%insert(key, h)
            end if
          case (2) ! Significant temperature level
            if (.not. record%snd_sig_pressure%hashed(key) .and. .not. is_missing(p)) then
              call record%snd_sig_pressure%insert(key, p)
              call record%snd_sig_pressure_qc%insert(key, p_qc)
            end if
            call prepbufr_raw(obs(3,i,:), T, stack_qc=qc(3,i,:), stack_pc=pc(3,i,:), qc=T_qc)
            if (.not. record%snd_sig_temperature%hashed(key) .and. .not. is_missing(T)) then
              call record%snd_sig_temperature%insert(key, T)
              call record%snd_sig_temperature_qc%insert(key, T_qc)
            end if
            call prepbufr_raw(obs(4,i,:), sh, stack_qc=qc(4,i,:), stack_pc=pc(4,i,:), qc=sh_qc)
            if (.not. record%snd_sig_specific_humidity%hashed(key) .and. .not. is_missing(sh)) then
              call record%snd_sig_specific_humidity%insert(key, sh)
              call record%snd_sig_humidity_qc%insert(key, sh_qc)
            end if
            call prepbufr_raw(obs(5,i,:), Td, stack_qc=qc(5,i,:), stack_pc=pc(5,i,:))
            if (.not. record%snd_sig_dewpoint%hashed(key)) then
              if (is_missing(Td)) Td = dewpoint(p, sh)
              if (.not. is_missing(Td)) call record%snd_sig_dewpoint%insert(key, Td)
            end if
            rh = relative_humidity(p, T, sh)
            if (.not. record%snd_sig_relative_humidity%hashed(key) .and. .not. is_missing(rh)) then
              call record%snd_sig_relative_humidity%insert(key, rh)
            end if
            call prepbufr_raw(obs(6,i,:), u, stack_qc=qc(6,i,:), stack_pc=pc(6,i,:), qc=uv_qc)
            call prepbufr_raw(obs(7,i,:), v, stack_qc=qc(6,i,:), stack_pc=pc(6,i,:))
            if (.not. record%snd_sig_wind_u%hashed(key) .and. .not. is_missing(u) .and. .not. is_missing(v)) then
              call record%snd_sig_wind_u%insert(key, u)
              call record%snd_sig_wind_v%insert(key, v)
              call record%snd_sig_wind_speed%insert(key, sqrt(u**2 + v**2))
              call record%snd_sig_wind_direction%insert(key, wind_direction(u, v))
              call record%snd_sig_wind_qc%insert(key, uv_qc)
            end if
            call prepbufr_raw(obs(8,i,:), h, stack_qc=qc(8,i,:), stack_pc=pc(8,i,:))
            if (.not. record%snd_sig_height%hashed(key) .and. .not. is_missing(h)) then
              call record%snd_sig_height%insert(key, h)
            end if
          case (3, 4) ! Winds-by-pressure level or Winds-by-height level
            if (.not. record%snd_wnd_pressure%hashed(key) .and. .not. is_missing(p)) then
              call record%snd_wnd_pressure%insert(key, p)
              call record%snd_wnd_pressure_qc%insert(key, p_qc)
            end if
            call prepbufr_raw(obs(6,i,:), u, stack_qc=qc(6,i,:), stack_pc=pc(6,i,:), qc=uv_qc)
            call prepbufr_raw(obs(7,i,:), v, stack_qc=qc(6,i,:), stack_pc=pc(6,i,:))
            if (.not. record%snd_wnd_wind_u%hashed(key) .and. .not. is_missing(u) .and. .not. is_missing(v)) then
              call record%snd_wnd_wind_u%insert(key, u)
              call record%snd_wnd_wind_v%insert(key, v)
              call record%snd_wnd_wind_speed%insert(key, sqrt(u**2 + v**2))
              call record%snd_wnd_wind_direction%insert(key, wind_direction(u, v))
              call record%snd_wnd_wind_qc%insert(key, uv_qc)
            end if
            call prepbufr_raw(obs(8,i,:), h, stack_qc=qc(8,i,:), stack_pc=pc(8,i,:))
            if (.not. record%snd_wnd_height%hashed(key) .and. .not. is_missing(h)) then
              call record%snd_wnd_height%insert(key, h)
            end if
          case (5) ! Tropopause level
            if (.not. record%snd_trop_pressure%hashed(key) .and. .not. is_missing(p)) then
              call record%snd_trop_pressure%insert(key, p)
              call record%snd_trop_pressure_qc%insert(key, p_qc)
            end if
            call prepbufr_raw(obs(3,i,:), T, stack_qc=qc(3,i,:), stack_pc=pc(3,i,:), qc=T_qc)
            if (.not. record%snd_trop_temperature%hashed(key) .and. .not. is_missing(T)) then
              call record%snd_trop_temperature%insert(key, T)
              call record%snd_trop_temperature_qc%insert(key, T_qc)
            end if
            call prepbufr_raw(obs(4,i,:), sh, stack_qc=qc(4,i,:), stack_pc=pc(4,i,:), qc=sh_qc)
            if (.not. record%snd_trop_specific_humidity%hashed(key) .and. .not. is_missing(sh)) then
              call record%snd_trop_specific_humidity%insert(key, sh)
              call record%snd_trop_humidity_qc%insert(key, sh_qc)
            end if
            call prepbufr_raw(obs(5,i,:), Td, stack_qc=qc(5,i,:), stack_pc=pc(5,i,:))
            if (.not. record%snd_trop_dewpoint%hashed(key)) then
              if (is_missing(Td)) Td = dewpoint(p, sh)
              if (.not. is_missing(Td)) call record%snd_trop_dewpoint%insert(key, Td)
            end if
            call prepbufr_raw(obs(6,i,:), u, stack_qc=qc(6,i,:), stack_pc=pc(6,i,:), qc=uv_qc)
            call prepbufr_raw(obs(7,i,:), v, stack_qc=qc(6,i,:), stack_pc=pc(6,i,:))
            if (.not. record%snd_trop_wind_u%hashed(key) .and. .not. is_missing(u) .and. .not. is_missing(v)) then
              call record%snd_trop_wind_u%insert(key, u)
              call record%snd_trop_wind_v%insert(key, v)
              call record%snd_trop_wind_speed%insert(key, sqrt(u**2 + v**2))
              call record%snd_trop_wind_direction%insert(key, wind_direction(u, v))
              call record%snd_trop_wind_qc%insert(key, uv_qc)
            end if
            call prepbufr_raw(obs(8,i,:), h, stack_qc=qc(8,i,:), stack_pc=pc(8,i,:))
            if (.not. record%snd_trop_height%hashed(key) .and. .not. is_missing(h)) then
              call record%snd_trop_height%insert(key, h)
            end if
          end select
        end do

        if (new_record) then
          call records%insert(station_name // '@' // time%isoformat(), record)
        end if
        ! if (station_name == '87344') then
        !   call debug_print(record, obs, qc, pc)
        ! end if
      end do
    end do
    call closbf(10)

  end subroutine raob_prepbufr_read

  subroutine debug_print(record, obs, qc, pc)

    type(raob_read_record_type), intent(in) :: record
    real(8), intent(in) :: obs(max_num_var,max_num_lev,max_num_event)
    real(8), intent(in) :: qc(max_num_var,max_num_lev,max_num_event)
    real(8), intent(in) :: pc(max_num_var,max_num_lev,max_num_event)

    integer num_level, i
    type(hash_table_iterator_type) level_iterator

    print *, 'Station ', record%station%name
    print *, 'Time ', record%time%isoformat()
    print *, '- Mandatory levels:'
    write(*, '(8A15)') 'P', 'H', 'T', 'SH', 'TD', 'RH', 'U', 'V'
    level_iterator = hash_table_iterator(record%snd_man_pressure)
    do while (.not. level_iterator%ended())
      select type (value => level_iterator%value)
      type is (real)
        write(*, '(F15.1)', advance='no') value
      class default
        write(*, '(15X)', advance='no')
      end select
      select type (value => record%snd_man_height%value(level_iterator%key))
      type is (real)
        write(*, '(F15.1)', advance='no') value
      class default
        write(*, '(15X)', advance='no')
      end select
      select type (value => record%snd_man_temperature%value(level_iterator%key))
      type is (real)
        write(*, '(F15.1)', advance='no') value
      class default
        write(*, '(15X)', advance='no')
      end select
      select type (value => record%snd_man_specific_humidity%value(level_iterator%key))
      type is (real)
        write(*, '(F15.1)', advance='no') value
      class default
        write(*, '(15X)', advance='no')
      end select
      select type (value => record%snd_man_dewpoint%value(level_iterator%key))
      type is (real)
        write(*, '(F15.1)', advance='no') value
      class default
        write(*, '(15X)', advance='no')
      end select
      select type (value => record%snd_man_relative_humidity%value(level_iterator%key))
      type is (real)
        write(*, '(F15.1)', advance='no') value
      class default
        write(*, '(15X)', advance='no')
      end select
      select type (value => record%snd_man_wind_u%value(level_iterator%key))
      type is (real)
        write(*, '(F15.1)', advance='no') value
      class default
        write(*, '(15X)', advance='no')
      end select
      select type (value => record%snd_man_wind_v%value(level_iterator%key))
      type is (real)
        write(*, '(F15.1)', advance='no') value
      class default
        write(*, '(15X)', advance='no')
      end select
      write(*, *)
      call level_iterator%next()
    end do
    print *, '- Significant levels:'
    write(*, '(8A15)') 'P', 'H', 'T', 'SH', 'TD', 'RH', 'U', 'V'
    level_iterator = hash_table_iterator(record%snd_sig_pressure)
    do while (.not. level_iterator%ended())
      select type (value => level_iterator%value)
      type is (real)
        write(*, '(F15.1)', advance='no') value
      class default
        write(*, '(15X)', advance='no')
      end select
      select type (value => record%snd_sig_height%value(level_iterator%key))
      type is (real)
        write(*, '(F15.1)', advance='no') value
      class default
        write(*, '(15X)', advance='no')
      end select
      select type (value => record%snd_sig_temperature%value(level_iterator%key))
      type is (real)
        write(*, '(F15.1)', advance='no') value
      class default
        write(*, '(15X)', advance='no')
      end select
      select type (value => record%snd_sig_specific_humidity%value(level_iterator%key))
      type is (real)
        write(*, '(F15.1)', advance='no') value
      class default
        write(*, '(15X)', advance='no')
      end select
      select type (value => record%snd_sig_dewpoint%value(level_iterator%key))
      type is (real)
        write(*, '(F15.1)', advance='no') value
      class default
        write(*, '(15X)', advance='no')
      end select
      select type (value => record%snd_sig_relative_humidity%value(level_iterator%key))
      type is (real)
        write(*, '(F15.1)', advance='no') value
      class default
        write(*, '(15X)', advance='no')
      end select
      select type (value => record%snd_sig_wind_u%value(level_iterator%key))
      type is (real)
        write(*, '(F15.1)', advance='no') value
      class default
        write(*, '(15X)', advance='no')
      end select
      select type (value => record%snd_sig_wind_v%value(level_iterator%key))
      type is (real)
        write(*, '(F15.1)', advance='no') value
      class default
        write(*, '(15X)', advance='no')
      end select
      write(*, *)
      call level_iterator%next()
    end do
    print *, '- Wind levels:'
    write(*, '(2A15, 60X, 2A15)') 'P', 'H', 'U', 'V'
    level_iterator = hash_table_iterator(record%snd_wnd_pressure)
    do while (.not. level_iterator%ended())
      select type (value => level_iterator%value)
      type is (real)
        write(*, '(F15.1)', advance='no') value
      class default
        write(*, '(15X)', advance='no')
      end select
      select type (value => record%snd_wnd_height%value(level_iterator%key))
      type is (real)
        write(*, '(F15.1)', advance='no') value
      class default
        write(*, '(15X)', advance='no')
      end select
      write(*, '(60X)', advance='no')
      select type (value => record%snd_wnd_wind_u%value(level_iterator%key))
      type is (real)
        write(*, '(F15.1)', advance='no') value
      class default
        write(*, '(15X)', advance='no')
      end select
      select type (value => record%snd_wnd_wind_v%value(level_iterator%key))
      type is (real)
        write(*, '(F15.1)', advance='no') value
      class default
        write(*, '(15X)', advance='no')
      end select
      write(*, *)
      call level_iterator%next()
    end do
    print *, '- Tropopause levels:'
    write(*, '(8A15)') 'P', 'H', 'T', 'SH', 'TD', 'RH', 'U', 'V'
    level_iterator = hash_table_iterator(record%snd_trop_pressure)
    do while (.not. level_iterator%ended())
      select type (value => level_iterator%value)
      type is (real)
        write(*, '(F15.1)', advance='no') value
      class default
        write(*, '(15X)', advance='no')
      end select
      select type (value => record%snd_trop_height%value(level_iterator%key))
      type is (real)
        write(*, '(F15.1)', advance='no') value
      class default
        write(*, '(15X)', advance='no')
      end select
      select type (value => record%snd_trop_temperature%value(level_iterator%key))
      type is (real)
        write(*, '(F15.1)', advance='no') value
      class default
        write(*, '(15X)', advance='no')
      end select
      select type (value => record%snd_trop_specific_humidity%value(level_iterator%key))
      type is (real)
        write(*, '(F15.1)', advance='no') value
      class default
        write(*, '(15X)', advance='no')
      end select
      select type (value => record%snd_trop_dewpoint%value(level_iterator%key))
      type is (real)
        write(*, '(F15.1)', advance='no') value
      class default
        write(*, '(15X)', advance='no')
      end select
      select type (value => record%snd_trop_relative_humidity%value(level_iterator%key))
      type is (real)
        write(*, '(F15.1)', advance='no') value
      class default
        write(*, '(15X)', advance='no')
      end select
      select type (value => record%snd_trop_wind_u%value(level_iterator%key))
      type is (real)
        write(*, '(F15.1)', advance='no') value
      class default
        write(*, '(15X)', advance='no')
      end select
      select type (value => record%snd_trop_wind_v%value(level_iterator%key))
      type is (real)
        write(*, '(F15.1)', advance='no') value
      class default
        write(*, '(15X)', advance='no')
      end select
      write(*, *)
      call level_iterator%next()
    end do

    ! num_level = prepbufr_value_count(obs(1,:,1))
    ! write(*, '(' // to_string(num_level) // 'I2)') int(obs(1,:num_level,1))
    ! do i = 1, num_level
    !   write(*, '(I2, 6F15.1)') int(obs(1,i,1)), obs(2,i,1), obs(3,i,1), obs(4,i,1), obs(5,i,1), obs(6,i,1), obs(7,i,1)
    ! end do
    ! print *, count(obs(1,:,1) == 1)

  end subroutine debug_print

end module
