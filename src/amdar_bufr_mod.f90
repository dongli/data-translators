! AMDAR: Aircraft Meteorological Data Relay

module amdar_bufr_mod

  use amdar_mod
  use datetime_mod
  use timedelta_mod
  use hash_table_mod
  use linked_list_mod
  use params_mod
  use utils_mod
  use eccodes
  use odbql_wrappers

  implicit none

  private

  public amdar_bufr_decode
  public amdar_bufr_write_odb

  type(hash_table_type)  flights
  type(linked_list_type) records

contains

  subroutine amdar_bufr_decode(file_path)

    character(*), intent(in) :: file_path

    integer file_id, bufr_id, ret, subset_id
    integer(4) num_subset
    character(10) flight_name
    integer year, month, day, hour, minute, second
    type(amdar_flight_type), pointer :: flight
    type(amdar_record_type), pointer :: record

    flights = hash_table(chunk_size=50000, max_load_factor=0.9)
    call records%clear()

    call codes_open_file(file_id, file_path, 'r')

    call codes_bufr_new_from_file(file_id, bufr_id, ret)

    do while (ret /= CODES_END_OF_FILE)
      call codes_set(bufr_id, 'unpack', 1)
      call codes_get(bufr_id, 'numberOfSubsets', num_subset)

      do subset_id = 1, num_subset
        call bufr_value(bufr_id, subset_id, 'aircraftFlightNumber', flight_name)

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
        allocate(record)
        record%flight => flight

        call bufr_value(bufr_id, subset_id, 'year', year)
        call bufr_value(bufr_id, subset_id, 'month', month)
        call bufr_value(bufr_id, subset_id, 'day', day)
        call bufr_value(bufr_id, subset_id, 'hour', hour)
        call bufr_value(bufr_id, subset_id, 'minute', minute)
        call bufr_value(bufr_id, subset_id, 'second', second)
        record%time = datetime(year, month, day, hour, minute, second)
        if (record%lon                     == real_missing_value) call bufr_value(bufr_id, subset_id, 'longitude',           record%lon)
        if (record%lat                     == real_missing_value) call bufr_value(bufr_id, subset_id, 'latitude',            record%lat)
        if (record%z                       == real_missing_value) call bufr_value(bufr_id, subset_id, 'flightLevel',         record%z)
        if (record%amdar_temperature       == real_missing_value) call bufr_value(bufr_id, subset_id, 'airTemperature',      record%amdar_temperature)
        if (record%amdar_wind_speed        == real_missing_value) call bufr_value(bufr_id, subset_id, 'windSpeed',           record%amdar_wind_speed)
        if (record%amdar_wind_direction    == real_missing_value) call bufr_value(bufr_id, subset_id, 'windDirection',       record%amdar_wind_direction)
        if (record%amdar_dewpoint          == real_missing_value) call bufr_value(bufr_id, subset_id, 'dewpointTemperature', record%amdar_dewpoint)
        if (record%amdar_specific_humidity == real_missing_value) call bufr_value(bufr_id, subset_id, 'mixingRatio',         record%amdar_specific_humidity)
        if (record%amdar_relative_humidity == real_missing_value) call bufr_value(bufr_id, subset_id, 'relativeHumidity',    record%amdar_relative_humidity)

        ! Convert units.
        if (record%amdar_temperature /= real_missing_value) record%amdar_temperature = record%amdar_temperature - 273.15

        call records%insert(flight_name // '@' // record%time%isoformat(), record)
      end do

      call codes_release(bufr_id)

      call codes_bufr_new_from_file(file_id, bufr_id, ret)
    end do

    call codes_close_file(file_id)

  end subroutine amdar_bufr_decode

  subroutine amdar_bufr_write_odb()

    ! ODB variables
    type(odbql) odb_db
    type(odbql_stmt) odb_stmt
    type(odbql_value) odb_value
    character(100) odb_unparsed_sql
    character(50) odb_file_name

    character(30) time_str
    type(linked_list_iterator_type) record_iterator

    odb_file_name = 'amdar.odb'

    ! Write ODB file.
    call odbql_open('', odb_db)
    call odbql_prepare_v2(odb_db, 'CREATE TABLE amdar AS (' // &
      'flight_name STRING, lon REAL, lat REAL, z REAL, time STRING, ' // &
      'amdar_temperature REAL, ' // &
      'amdar_wind_speed REAL, ' // &
      'amdar_wind_direction REAL, ' // &
      'amdar_specific_humidity REAL) ON "' // &
      trim(odb_file_name) // '";', -1, odb_stmt, odb_unparsed_sql)
    call odbql_prepare_v2(odb_db, 'INSERT INTO amdar (' // &
      'flight_name, lon, lat, z, time, ' // &
      'amdar_temperature, ' // &
      'amdar_wind_speed, ' // &
      'amdar_wind_direction, ' // &
      'amdar_specific_humidity' // &
      ') VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?);', -1, odb_stmt, odb_unparsed_sql)

    record_iterator = linked_list_iterator(records)
    do while (.not. record_iterator%ended())
      select type (record => record_iterator%value)
      type is (amdar_record_type)
        call odbql_bind_text  (odb_stmt, 1, record%flight%name, len_trim(record%flight%name))
        call odbql_bind_double(odb_stmt, 2, dble(record%lon))
        call odbql_bind_double(odb_stmt, 3, dble(record%lat))
        call odbql_bind_double(odb_stmt, 4, dble(record%z))
        time_str = record%time%format('%Y%m%d%H%M')
        time_str = 'N/A'
        call odbql_bind_text  (odb_stmt, 5, time_str, len_trim(time_str))
        call odbql_bind_double(odb_stmt, 6, dble(record%amdar_temperature))
        call odbql_bind_double(odb_stmt, 7, dble(record%amdar_wind_speed))
        call odbql_bind_double(odb_stmt, 8, dble(record%amdar_wind_direction))
        call odbql_bind_double(odb_stmt, 9, dble(record%amdar_specific_humidity))
        call odbql_step(odb_stmt)
      class default
        write(*, *) '[Error]: Unknown record in the list!'
        stop 1
      end select
      call record_iterator%next()
    end do

    call odbql_finalize(odb_stmt)
    call odbql_close(odb_db)
    write(*, *) '[Notice]: ODB file is written.'

  end subroutine amdar_bufr_write_odb

end module amdar_bufr_mod
