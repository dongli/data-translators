module ship_cimiss_txt_mod

  use datetime
  use ship_mod
  use hash_table_mod
  use linked_list_mod
  use regex
  use params_mod
  use utils_mod

  implicit none

  private

  public ship_cimiss_txt_read

contains

  subroutine ship_cimiss_txt_read(file_path)

    character(*), intent(in) :: file_path

    character(1024) line
    integer i, j, iostat
    type(reg_matches), allocatable :: res(:)
    character(30), allocatable :: elements(:)
    character(10) ship_name
    real lat, lon
    real ws,  ws_qc
    real wd,  wd_qc
    real Ta,  Ta_qc
    real Td,  Td_qc
    real RH,  RH_qc
    real p,   p_qc
    real slp, slp_qc
    real SST, SST_qc
    integer type, year, month, day, hour, min
    type(datetime_type) time
    logical new_record
    type(ship_type), pointer :: ship
    type(ship_record_type), pointer :: record

    ships = hash_table(chunk_size=50000, max_load_factor=0.9)
    call records%clear()

    open(10, file=file_path, action='read', form='formatted')
    ! Header line
    read(10, '(A)') line
    res = regex_search(line, 'returnMessage="([^"]*)"')
    if (size(res) /= 1) then
      write(*, *) '[Error]: Bad CIMISS text file without matched returnMessage token!'
      stop 1
    else if (res(1)%match(2)%str /= 'Query Succeed') then
      write(*, *) '[Error]: Failed CIMISS query! ' // trim(res(1)%match(2)%str)
      stop 1
    end if
    ! Elements line
    read(10, '(A)') line
    res = regex_search(line, '(\w+)')
    if (size(res) > 0) then
      allocate(elements(size(res)))
      do i = 1, size(res)
        elements(i) = res(i)%match(1)%str
      end do
    else
      write(*, *) '[Error]: Bad CIMISS text file without matched elements line!'
      stop 1
    end if
    ! Record line
    do while (.true.)
      read(10, '(A)', iostat=iostat) line
      res = regex_search(line, '([^\s]+)')
      if (iostat /= 0) exit
      ship_name = ''
      lat    = real_missing_value
      lon    = real_missing_value
      year   = int_missing_value
      month  = int_missing_value
      day    = int_missing_value
      hour   = int_missing_value
      min    = int_missing_value
      wd     = real_missing_value
      wd_qc  = int_missing_value
      ws     = real_missing_value
      ws_qc  = int_missing_value
      Ta     = real_missing_value
      Ta_qc  = int_missing_value
      Td     = real_missing_value
      Td_qc  = int_missing_value
      RH     = real_missing_value
      RH_qc  = int_missing_value
      p      = real_missing_value
      p_qc   = int_missing_value
      slp    = real_missing_value
      slp_qc = int_missing_value
      SST    = real_missing_value
      SST_qc = int_missing_value
      do i = 1, size(elements)
        select case (elements(i))
        case ('Station_Id_C')
          ship_name = res(i)%match(1)%str
        case ('Lat')
          read(res(i)%match(1)%str, *) lat
        case ('Lon')
          read(res(i)%match(1)%str, *) lon
        case ('Station_Type')
          read(res(i)%match(1)%str, *) type
        case ('Year')
          read(res(i)%match(1)%str, *) year
        case ('Mon')
          read(res(i)%match(1)%str, *) month
        case ('Day')
          read(res(i)%match(1)%str, *) day
        case ('Hour')
          read(res(i)%match(1)%str, *) hour
        case ('Min')
          read(res(i)%match(1)%str, *) min
        case ('WIN_D')
          read(res(i)%match(1)%str, *) wd
        case ('Q_WIN_D')
          read(res(i)%match(1)%str, *) wd_qc
        case ('WIN_S')
          read(res(i)%match(1)%str, *) ws
        case ('Q_WIN_S')
          read(res(i)%match(1)%str, *) ws_qc
        case ('TEM')
          read(res(i)%match(1)%str, *) Ta
        case ('Q_TEM')
          read(res(i)%match(1)%str, *) Ta_qc
        case ('DPT')
          read(res(i)%match(1)%str, *) Td
        case ('Q_DPT')
          read(res(i)%match(1)%str, *) Td_qc
        case ('RHU')
          read(res(i)%match(1)%str, *) RH
        case ('Q_RHU')
          read(res(i)%match(1)%str, *) RH_qc
        case ('PRS')
          read(res(i)%match(1)%str, *) p
        case ('Q_PRS')
          read(res(i)%match(1)%str, *) p_qc
        case ('PRS_Sea')
          read(res(i)%match(1)%str, *) slp
        case ('Q_PRS_Sea')
          read(res(i)%match(1)%str, *) slp_qc
        case ('SST')
          read(res(i)%match(1)%str, *) SST
        case ('Q_SST')
          read(res(i)%match(1)%str, *) SST_qc
        end select
      end do
      time = create_datetime(year, month, day, hour, min)
      if (ships%hashed(ship_name)) then
        select type (value => ships%value(ship_name))
        type is (ship_type)
          ship => value
        end select
      else
        allocate(ship)
        ship%name = ship_name
        call ships%insert(ship_name, ship)
      end if
      nullify(record)
      select type (value => records%last_value())
      type is (ship_record_type)
        record => value
        if (record%ship%name == ship_name .and. record%time == time) then
          new_record = .false.
        else
          nullify(record)
        end if
      end select
      if (.not. associated(record)) then
        allocate(record)
        record%ship => ship
        record%time = time
        new_record = .true.
      end if
      record%time = time
      record%lon = lon
      record%lat = lat
      record%ship_pressure = p
      record%ship_pressure_qc = p_qc
      record%ship_air_temperature = Ta
      record%ship_air_temperature_qc = Ta_qc
      record%ship_sea_temperature = SST
      record%ship_sea_temperature_qc = SST_qc
      record%ship_dewpoint = Td
      record%ship_dewpoint_qc = Td_qc
      record%ship_relative_humidity = RH
      record%ship_relative_humidity_qc = RH_qc
      record%ship_wind_speed = ws
      record%ship_wind_direction = wd
      record%ship_wind_qc = ws_qc

      if (new_record) then
        call records%insert(ship_name // '@' // time%isoformat(), record)
      end if
      ! if (ship_name == '40718') call debug_print(record)
    end do
    close(10)

  end subroutine ship_cimiss_txt_read

  subroutine debug_print(record)

    type(ship_record_type), intent(in) :: record

    print *, '--'
    print *, record%ship%name, record%time%isoformat()
    print *, 'lon: ', record%lon
    print *, 'lat: ', record%lat
    print *, 'p:   ', record%ship_pressure, record%ship_pressure_qc
    print *, 'Ta:  ', record%ship_air_temperature, record%ship_air_temperature_qc
    print *, 'Td:  ', record%ship_dewpoint, record%ship_dewpoint_qc
    print *, 'SST: ', record%ship_sea_temperature, record%ship_sea_temperature_qc
    print *, 'RH:  ', record%ship_relative_humidity, record%ship_relative_humidity_qc
    print *, 'WS:  ', record%ship_wind_speed, record%ship_wind_qc
    print *, 'WD:  ', record%ship_wind_direction, record%ship_wind_qc

  end subroutine debug_print

end module ship_cimiss_txt_mod