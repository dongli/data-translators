module ship_txt_mod

  use datetime
  use ship_mod
  use hash_table_mod
  use linked_list_mod
  use regex
  use params_mod
  use utils_mod

  implicit none

  private

  public ship_txt_read

contains

  subroutine ship_txt_read(file_path, ships, records)

    character(*), intent(in) :: file_path
    type(hash_table_type), intent(inout) :: ships
    type(linked_list_type), intent(inout) :: records

    type(ship_type), pointer :: ship
    type(ship_record_type), pointer :: record
    type(datetime_type) time
    character(1024) line
    integer ierr
    integer source
    integer year
    integer month
    integer day
    integer hour
    integer minute
    real lat
    real lon
    character(20) site_name
    integer site_type

    real move_direction        ! deg
    real move_speed            ! m/s
    real sea_sfc_pressure      ! Pa
    real air_temperature       ! degC
    real dewpoint              ! degC
    real relative_humidity     ! %
    real wind_direction        ! deg
    real wind_speed            ! m/s
    real sea_sfc_temperature   ! degC
    integer ice_cover
    real wind_u                ! m/s
    real wind_v                ! m/s
    real specific_humidity     ! mg/kg
    real altitude              ! m
    
    integer sea_sfc_pressure_qc
    integer air_temperature_qc
    integer dewpoint_qc
    integer relative_humidity_qc
    integer wind_direction_qc
    integer wind_speed_qc
    integer sea_sfc_temperature_qc
    integer ice_cover_qc
    integer wind_u_qc
    integer wind_v_qc
    integer specific_humidity_qc
    integer encoding_type_qc
    integer wind_qc ! FIXME: Merge wind_*_qc into one QC mark.
 
    ships = hash_table(chunk_size=50000, max_load_factor=0.9)
    call records%clear()

    write(*, *) '[Notice]: Reading ' // trim(file_path) // ' ...'

    open(10, file=file_path, action='read', form='formatted')
    ! Header line
    read(10, '(A)') line
    do while (.true.)
      read(10, '(A)', iostat=ierr) line
      if (ierr == 0) then
        read(line, *) source,                   & ! 1
                      year,                     & ! 2
                      month,                    & ! 3
                      day,                      & ! 4
                      hour,                     & ! 5
                      minute,                   & ! 6
                      lat,                      & ! 7
                      lon,                      & ! 8
                      site_name,                & ! 9
                      site_type,                & ! 10
                      move_direction,           & ! 11
                      move_speed,               & ! 12
                      sea_sfc_pressure,         & ! 13
                      air_temperature,          & ! 14
                      dewpoint,                 & ! 15
                      relative_humidity,        & ! 16
                      wind_direction,           & ! 17
                      wind_speed,               & ! 18
                      sea_sfc_temperature,      & ! 19
                      ice_cover,                & ! 20
                      wind_u,                   & ! 21
                      wind_v,                   & ! 22
                      specific_humidity,        & ! 23
                      altitude,                 & ! 24
                      sea_sfc_pressure_qc,      & ! 25
                      air_temperature_qc,       & ! 26
                      dewpoint_qc,              & ! 27
                      relative_humidity_qc,     & ! 28
                      wind_direction_qc,        & ! 29
                      wind_speed_qc,            & ! 30
                      sea_sfc_temperature_qc,   & ! 31
                      ice_cover_qc,             & ! 32
                      wind_u_qc,                & ! 33
                      wind_v_qc,                & ! 34
                      specific_humidity_qc,     & ! 35
                      encoding_type_qc            ! 36

        time = create_datetime(year=year, month=month, day=day, hour=hour, minute=minute)
        source = merge(int_missing_value, source, is_missing(source, src='cimiss'))
        lat = merge(real_missing_value, lat, is_missing(lat, src='cimiss'))
        lon = merge(real_missing_value, lon, is_missing(lon, src='cimiss'))
        site_type = merge(int_missing_value, site_type, is_missing(site_type, src='cimiss'))
        move_direction = merge(real_missing_value, move_direction, is_missing(move_direction, src='cimiss'))
        move_speed = merge(real_missing_value, move_speed, is_missing(move_speed, src='cimiss'))
        sea_sfc_pressure = multiply(merge(real_missing_value, sea_sfc_pressure, is_missing(sea_sfc_pressure, src='cimiss')), 100.0)
        air_temperature = merge(real_missing_value, air_temperature, is_missing(air_temperature, src='cimiss'))
        dewpoint = merge(real_missing_value, dewpoint, is_missing(dewpoint, src='cimiss'))
        relative_humidity = merge(real_missing_value, relative_humidity, is_missing(relative_humidity, src='cimiss'))
        wind_direction = merge(real_missing_value, wind_direction, is_missing(wind_direction, src='cimiss'))
        wind_speed = merge(real_missing_value, wind_speed, is_missing(wind_speed, src='cimiss'))
        sea_sfc_temperature = merge(real_missing_value, sea_sfc_temperature, is_missing(sea_sfc_temperature, src='cimiss'))
        ice_cover = merge(int_missing_value, ice_cover, is_missing(ice_cover, src='cimiss'))
        wind_u = merge(real_missing_value, wind_u, is_missing(wind_u, src='cimiss'))
        wind_v = merge(real_missing_value, wind_v, is_missing(wind_v, src='cimiss'))
        specific_humidity = merge(real_missing_value, specific_humidity, is_missing(specific_humidity, src='cimiss'))
        altitude = merge(real_missing_value, altitude, is_missing(altitude, src='cimiss'))
        sea_sfc_pressure_qc = merge(int_missing_value, sea_sfc_pressure_qc, is_missing(sea_sfc_pressure_qc, src='cimiss'))
        air_temperature_qc = merge(int_missing_value, air_temperature_qc, is_missing(air_temperature_qc, src='cimiss'))
        dewpoint_qc = merge(int_missing_value, dewpoint_qc, is_missing(dewpoint_qc, src='cimiss'))
        relative_humidity_qc = merge(int_missing_value, relative_humidity_qc, is_missing(relative_humidity_qc, src='cimiss'))
        wind_direction_qc = merge(int_missing_value, wind_direction_qc, is_missing(wind_direction_qc, src='cimiss'))
        wind_speed_qc = merge(int_missing_value, wind_speed_qc, is_missing(wind_speed_qc, src='cimiss'))
        sea_sfc_temperature_qc = merge(int_missing_value, sea_sfc_temperature_qc, is_missing(sea_sfc_temperature_qc, src='cimiss'))
        ice_cover_qc = merge(int_missing_value, ice_cover_qc, is_missing(ice_cover_qc, src='cimiss'))
        if (is_missing(wind_u_qc, src='cimiss') .or. is_missing(wind_v_qc, src='cimiss')) then
          wind_qc = int_missing_value
        else if (wind_u_qc /= 0) then
          wind_qc = wind_u_qc
        else if (wind_v_qc /= 0) then
          wind_qc = wind_v_qc
        else
          wind_qc = 0
        end if
        specific_humidity_qc = merge(int_missing_value, specific_humidity_qc, is_missing(specific_humidity_qc, src='cimiss'))
        encoding_type_qc = merge(int_missing_value, encoding_type_qc, is_missing(encoding_type_qc, src='cimiss'))
        ! Create ship and record.
        if (ships%hashed(site_name)) then
          select type (value => ships%value(site_name))
          type is (ship_type)
            ship => value
          end select
        else
          allocate(ship)
          call ship%init(site_name)
          ship%seq_id = ships%size
          call ships%insert(site_name, ship)
        end if
        allocate(record)
        record%seq_id = records%size
        record%ship => ship
        record%time = time
        ! Set record.
        select case (source)
        case (1)
          record%source = 'GTS'
        case (2)
          record%source = 'ICOADS'
        case (3)
          record%source = 'CFSRGDAS'
        case (4)
          record%source = 'CMA BUOY'
        case (5)
          record%source = 'CHN SHIP'
        case (6)
          record%source = 'CMAILAWS'
        case (9)
          record%source = 'MULTISRC'
        end select
        record%lon = lon
        record%lat = lat
        record%pressure = sea_sfc_pressure
        record%air_temperature = air_temperature
        record%sea_temperature = sea_sfc_temperature
        record%dewpoint = dewpoint
        record%relative_humidity = relative_humidity
        record%specific_humidity = specific_humidity
        record%wind_u = wind_u
        record%wind_v = wind_v
        record%wind_direction = wind_direction
        record%wind_speed = wind_speed
        record%ice_cover = ice_cover
        ! TODO: How to map CIMISS QC to PrepBUFR QC?
        record%pressure_qc = sea_sfc_pressure_qc
        record%air_temperature_qc = air_temperature_qc
        record%sea_temperature_qc = sea_sfc_temperature_qc
        record%dewpoint_qc = dewpoint_qc
        record%relative_humidity_qc = relative_humidity_qc
        record%specific_humidity_qc = specific_humidity_qc
        record%wind_qc = wind_qc
        record%ice_cover_qc = ice_cover_qc
        call records%insert(site_name // '@' // time%isoformat(), record)
        call ship%records%insert(trim(to_string(record%seq_id)), record, nodup=.true.)
      else
        exit
      end if
    end do
    close(10)

    write(*, *) '[Notice]: Ship size is ' // trim(to_string(ships%size)) // ', record size is ' // trim(to_string(records%size)) // '.'

  end subroutine ship_txt_read

end module ship_txt_mod
