module synop_a_txt_mod

  use datetime
  use string
  use container
  use flogger
  use regex
  use params_mod
  use utils_mod
  use synop_mod

  implicit none

  private

  public synop_a_txt_read

contains

  subroutine synop_a_txt_read(file_path, stations, records)

    character(*), intent(in) :: file_path
    type(hash_table_type), intent(inout), target :: stations
    type(linked_list_type), intent(inout), target :: records

    integer fu
    character(30) dummy
    character(8) station_name, lon_str, lat_str, alt_str, data_str(16)
    integer qc, year, month, i, j, k
    character(3) head
    real lon, lat, alt
    type(datetime_type) base_time, time
    type(timedelta_type) hour
    type(synop_station_type), pointer :: station
    type(synop_record_type), pointer :: record
    type(hash_table_type) local_records
    type(hash_table_iterator_type) record_iterator

    fu = unique_file_number()
    hour = create_timedelta(hours=1)
    local_records = hash_table(chunk_size=50000, max_load_factor=0.9)

    open(fu, file=file_path, status='old')
    read(fu, *) station_name, lat_str, lon_str, alt_str, &
                dummy, dummy, dummy, dummy, dummy, qc, year, month
    base_time = create_datetime(year=year, month=month)
    read(lat_str(1:4), '(F4.0)') lat; lat = lat * 0.01; if (lat_str(5:5) == 'S') lat = -lat
    read(lon_str(1:5), '(F5.0)') lon; lon = lon * 0.01; if (lon_str(6:6) == 'W') lon = -lon
    read(alt_str(2:6), '(F5.0)') alt; alt = alt * 0.1
    ! Create station or use hashed one.
    if (stations%hashed(station_name)) then
      select type (value => stations%value(station_name))
      type is (synop_station_type)
        station => value
      end select
    else
      allocate(station)
      call station%init(station_name, lon, lat, alt)
      station%seq_id = stations%size
      call stations%insert(station_name, station)
    end if
    ! Pressure (P)
    read(fu, '(A)') head
    if (head(2:2) /= '=' .and. head(3:3) /= '=') then
      time = base_time - create_timedelta(hours=4)
      select case (head(2:2))
      case ('3')
      case ('4')
      case ('6')
      case ('8')
      case ('B')
      case ('C')
        do i = 1, days_of_month(year, month, datetime_gregorian_calendar)
          read(fu, '(12(A4, 1X))') data_str(1:12)
          do j = 1, 12
            time = time + hour
            if ( data_str(j) /= '////') then
              record => get_record(local_records, time, station)
              read(data_str(j), *) record%p; record%p = record%p * 0.1
            end if
          end do
          read(fu, '(12(A4, 1X))') data_str(1:12) ! There are four more data.
          do j = 1, 12
            time = time + hour
            if (data_str(j) /= '////') then
              record => get_record(local_records, time, station)
              read(data_str(j), *) record%p; record%p = record%p * 0.1
            end if
          end do
        end do
        read(fu, '(A)', advance='no') head(1:1)
        backspace(fu)
        if (head(1:1) /= '=' ) then
          ! Sea level pressure , 4 times / day
          do i = 1, days_of_month(year, month, datetime_gregorian_calendar)
            read(fu, *) data_str(1:4)
          end do
        else
          read(fu, *)
        end if
      case ('D')
      end select
    end if
    ! Temperature (T)
    read(fu, '(A)') head
    if (head(2:2) /= '=' .and. head(3:3) /= '=') then
      time = base_time - create_timedelta(hours=4)
      select case (head(2:2))
      case ('2')
      case ('7')
      case ('8')
      case ('B')
        do i = 1, days_of_month(year, month, datetime_gregorian_calendar)
          read(fu, '(12(A4, 1X))') data_str(1:12)
          do j = 1, 12
            time = time + hour
            if (data_str(j) /= '////') then
              record => get_record(local_records, time, station)
              read(data_str(j), *) record%ta; record%ta = record%ta * 0.1
            end if
          end do
          read(fu, '(12(A4, 1X))') data_str(1:12) ! There are four more data.
          do j = 1, 12
            time = time + hour
            if (data_str(j) /= '////') then
              record => get_record(local_records, time, station)
              read(data_str(j), *) record%ta; record%ta = record%ta * 0.1
            end if
          end do
        end do
      end select
    end if
    ! Wet bulb temperature (I)
    read(fu, '(A)') head
    if (head(2:2) /= '=' .and. head(3:3) /= '=') then
      time = base_time - create_timedelta(hours=4)
      select case (head(2:2))
      case ('2')
      case ('7')
      case ('8')
      case ('B')
        read(fu, *) head
        if (head /= '=') then
          call log_error('Handle IB.')
        end if
        ! Dewpoint temperature
        do i = 1, days_of_month(year, month, datetime_gregorian_calendar)
          do j = 1, 2
            read(fu, '(12(A4, 1X))') data_str(1:12)
            do k = 1, 12
              time = time + hour
              if (data_str(k) /= '////') then
                record => get_record(local_records, time, station)
                read(data_str(k)(1:4), *) record%td; record%td = record%td * 0.1
              end if
            end do
          end do
        end do
      end select
    end if
    ! Water vapour pressure (E)
    read(fu, '(A)') head
    if (head(2:2) /= '=' .and. head(3:3) /= '=') then
      time = base_time - create_timedelta(hours=4)
      select case (head(2:2))
      case ('0')
      case ('9')
      case ('A')
        do i = 1, days_of_month(year, month, datetime_gregorian_calendar)
          do j = 1, 2
            read(fu, *) data_str(1:12)
          end do
        end do
      end select
    end if
    ! Relative humidity (U)
    read(fu, '(A)') head
    if (head(2:2) /= '=' .and. head(3:3) /= '=') then
      time = base_time - create_timedelta(hours=4)
      select case (head(2:2))
      case ('0')
      case ('2')
      case ('7')
      case ('9')
      case ('A')
      case ('B')
        do i = 1, days_of_month(year, month, datetime_gregorian_calendar)
          read(fu, '(12(A2, 1X))') data_str(1:12)
          do j = 1, 12
            time = time + hour
            if (data_str(j) /= '////') then
              record => get_record(local_records, time, station)
              if (data_str(j) == '%%') then
                record%rh = 100
              else
                read(data_str(j)(1:2), *) record%rh
              end if
            end if
          end do
          read(fu, '(12(A2, 1X))') data_str(1:12) ! There are two more data.
          do j = 1, 12
            time = time + hour
            if (data_str(j) /= '////') then
              record => get_record(local_records, time, station)
              if (data_str(j) == '%%') then
                record%rh = 100
              else
                read(data_str(j)(1:2), *) record%rh
              end if
            end if
          end do
        end do
      end select
    end if
    ! Cloud amount (N)
    read(fu, '(A)') head
    if (head(2:2) /= '=' .and. head(3:3) /= '=') then
      time = base_time - create_timedelta(hours=4)
      select case (head(2:2))
      case ('0')
      case ('9')
      case ('A')
      end select
    end if
    ! Cloud height (H)
    read(fu, '(A)') head
    if (head(2:2) /= '=' .and. head(3:3) /= '=') then
      time = base_time - create_timedelta(hours=4)
      select case (head(2:2))
      case ('0')
      case ('9')
      case ('B')
      end select
    end if
    ! Cloud type (C)
    read(fu, '(A)') head
    if (head(2:2) /= '=' .and. head(3:3) /= '=') then
      time = base_time - create_timedelta(hours=4)
      select case (head(2:2))
      case ('0')
      case ('9')
      case ('A')
      end select
    end if
    ! Visibility (V)
    read(fu, '(A)') head
    if (head(2:2) /= '=' .and. head(3:3) /= '=') then
      time = base_time - create_timedelta(hours=4)
      select case (head(2:2))
      case ('0')
      case ('7')
      case ('8')
      case ('9')
      case ('A')
      case ('B')
        do i = 1, days_of_month(year, month, datetime_gregorian_calendar)
          read(fu, '(12(A4, 1X))') data_str(1:12)
          do j = 1, 12
            time = time + hour
            if (data_str(j) /= '////') then
              record => get_record(local_records, time, station)
              read(data_str(j), *) record%vis; record%vis = record%vis * 0.001
            end if
          end do
          read(fu, '(12(A4, 1X))') data_str(1:12) ! There are two more data.
          do j = 1, 12
            time = time + hour
            if (data_str(j) /= '////') then
              record => get_record(local_records, time, station)
              read(data_str(j), *) record%vis; record%vis = record%vis * 0.001
            end if
          end do
        end do
      end select
    end if
    ! Rain (R)
    read(fu, '(A)') head
    if (head(2:2) /= '=' .and. head(3:3) /= '=') then
      time = base_time - create_timedelta(hours=4)
      select case (head(2:2))
      case ('0')
      case ('2')
      case ('6')
        read(fu, '(A)', advance='no') head(1:1)
        backspace(fu)
        if (head(1:1) /= '=') then
          do i = 1, days_of_month(year, month, datetime_gregorian_calendar)
            ! 20-08, 08-20, 20-20
            read(fu, '(3(A4, 1X))') data_str(1:3)
          end do
        else
          read(fu, *)
        end if
        read(fu, '(A)', advance='no') head(1:1)
        backspace(fu)
        if (head(1:1) /= '=' ) then
          do i = 1, days_of_month(year, month, datetime_gregorian_calendar)
            do j = 1, 2
              read(fu, '(12(A4, 1X))') data_str(1:12)
              time = time + hour
              if (data_str(j) /= '////') then
                record => get_record(local_records, time, station)
                read(data_str(j), *) record%r01h; record%r01h = record%r01h * 0.1
              end if
            end do
          end do
        else
          read(fu, '(A)') head(1:1)
        end if
        read(fu, *)
        
      end select
    end if
    ! Weather (W)
    read(fu, '(A)') head
    if (head(2:2) /= '=' .and. head(3:3) /= '=') then
      time = base_time - create_timedelta(hours=4)
      select case (head(2:2))
      case ('0')
      end select
    end if
    ! Evaporation (L)
    read(fu, '(A)') head
    if (head(2:2) /= '=') then
      time = base_time - create_timedelta(hours=4)
      select case (head(2:2))
      case ('0')
      case ('A')
        read(fu,'(A)') head(1:1)
        backspace(fu)
        if (head(1:1) /= '=') then
          print*, 'Need parse LA'
          stop
        else
          read(fu,*)
          read(fu,*)
        end if
      case ('B')
      end select
    end if
    ! Snow (Z)
    read(fu, '(A)') head
    if (head(2:2) /= '=' .and. head(3:3) /= '=') then
      time = base_time - create_timedelta(hours=4)
      select case (head(2:2))
      case ('0')
      end select
    end if
    ! Electrical wire icing (G)
    read(fu, '(A)') head
    if (head(2:2) /= '=' .and. head(3:3) /= '=') then
      time = base_time - create_timedelta(hours=4)
      select case (head(2:2))
      case ('0')
      case ('2')
      end select
    end if
    ! Wind (F)
    read(fu, '(A)') head
    if (head(2:2) /= '=' .and. head(3:3) /= '=') then
      time = base_time - create_timedelta(hours=4)
      select case (head(2:2))
      case ('E')
      case ('H')
      case ('K')
      case ('N')
        do i = 1, days_of_month(year, month, datetime_gregorian_calendar)
          do j = 1, 4
            read(fu, '(6(A6, 1X))') data_str(1:6)
            do k = 1, 6
              time = time + hour
              if (data_str(k) /= '//////') then
                record => get_record(local_records, time, station)
                if (data_str(k)(1:3) /= 'PPC') then
                  read(data_str(k)(1:3), *) record%wd
                end if
                read(data_str(k)(4:6), *) record%ws; record%ws = record%ws * 0.1
                record%ua = wind_u_component(record%ws, record%wd)
                record%va = wind_v_component(record%ws, record%wd)
              end if
            end do
          end do
        end do
        do i = 1, days_of_month(year, month, datetime_gregorian_calendar)
          do j = 1, 4
            read(fu, *) data_str(1:6)
          end do
        end do
        ! Maximum wind speed
        do i = 1, days_of_month(year, month, datetime_gregorian_calendar)
          read(fu, *) data_str(1:4)
        end do
      end select
    end if
    ! Shallow surface skin temperature (D)
    read(fu, '(A)') head
    if (head(1:2) /= 'DB') then
      print*, 'Program exit. This is DB position, but now is: ', head(1:2)
      stop
    end if
    if (head(2:2) /= '=' .and. head(3:3) /= '=') then
      time = base_time - create_timedelta(hours=4)
      select case (head(2:2))
      case ('0')
      case ('1')
      case ('2')
      case ('7')
      case ('8')
      case ('9')
      case ('B')
      end select
    end if
    ! Deep surface skin temperature (K)
    read(fu, '(A)') head
    if (head(2:2) /= '=' .and. head(3:3) /= '=') then
      time = base_time - create_timedelta(hours=4)
      select case (head(2:2))
      case ('0')
      case ('1')
      case ('B')
      end select
    end if
    ! Frozen soil depth (A)
    read(fu, '(A)') head
    if (head(2:2) /= '=' .and. head(3:3) /= '=') then
      time = base_time - create_timedelta(hours=4)
      select case (head(2:2))
      case ('0')
      case ('6')
      end select
    end if
    ! Solar (S)
    read(fu, '(A)') head
    if (head(2:2) /= '=' .and. head(3:3) /= '=') then
      time = base_time - create_timedelta(hours=4)
      select case (head(2:2))
      case ('0')
      case ('2')
      case ('A')
      end select
    end if
    ! Grass (snow) surface temperature (B)
    read(fu, '(A)') head
    if (head(2:2) /= '=' .and. head(3:3) /= '=') then
      time = base_time - create_timedelta(hours=4)
      select case (head(2:2))
      case ('A')
      end select
    end if
    close(fu)

    record_iterator = hash_table_iterator(local_records)
    do while (.not. record_iterator%ended())
      select type (record => record_iterator%value)
      type is (synop_record_type)
        call records%insert(record)
      end select
      call record_iterator%next()
    end do

    call log_notice('Station size is ' // trim(to_string(stations%size)) // ', record size is ' // trim(to_string(records%size)) // '.')

  end subroutine synop_a_txt_read

  function get_record(records, time, station) result(res)

    type(hash_table_type), intent(inout) :: records
    type(datetime_type), intent(in) :: time
    type(synop_station_type), intent(in), target :: station
    type(synop_record_type), pointer :: res

    if (.not. records%hashed(time%isoformat())) then
      allocate(res)
      res%station => station
      res%time = time
      call records%insert(time%isoformat(), res)
    end if
    select type (value => records%value(time%isoformat()))
    type is (synop_record_type)
      res => value
    class default
      call log_error('Internal error!', __FILE__, __LINE__)
    end select

  end function get_record

end module synop_a_txt_mod
