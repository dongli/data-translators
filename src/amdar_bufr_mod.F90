! AMDAR: Aircraft Meteorological Data Relay

module amdar_bufr_mod

  use amdar_mod
  use datetime
  use hash_table_mod
  use linked_list_mod
  use params_mod
  use utils_mod
  use eccodes

  implicit none

  private

  public amdar_bufr_read

contains

  subroutine amdar_bufr_read(file_path, flights, records)

    character(*), intent(in) :: file_path
    type(hash_table_type), intent(inout) :: flights
    type(linked_list_type), intent(inout) :: records

    integer file_id, bufr_id, ret, subset_id
    integer(4) num_subset
    character(10) flight_name
    integer year, month, day, hour, minute, second
    type(amdar_flight_type), pointer :: flight
    type(amdar_record_type), pointer :: record

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
        record%time = create_datetime(year, month, day, hour, minute, second)
        if (is_missing(record%lon)) call bufr_value(bufr_id, subset_id, 'longitude',           record%lon)
        if (is_missing(record%lat)) call bufr_value(bufr_id, subset_id, 'latitude',            record%lat)
        if (is_missing(record%z))   call bufr_value(bufr_id, subset_id, 'flightLevel',         record%z)
        if (is_missing(record%ta))  call bufr_value(bufr_id, subset_id, 'airTemperature',      record%ta)
        if (is_missing(record%ws))  call bufr_value(bufr_id, subset_id, 'windSpeed',           record%ws)
        if (is_missing(record%wd))  call bufr_value(bufr_id, subset_id, 'windDirection',       record%wd)
        if (is_missing(record%td))  call bufr_value(bufr_id, subset_id, 'dewpointTemperature', record%td)
        if (is_missing(record%sh))  call bufr_value(bufr_id, subset_id, 'mixingRatio',         record%sh)
        if (is_missing(record%rh))  call bufr_value(bufr_id, subset_id, 'relativeHumidity',    record%rh)

        ! Convert units.
        if (record%ta /= real_missing_value) record%ta = record%ta - 273.15

        call records%insert(flight_name // '@' // record%time%isoformat(), record)
      end do

      call codes_release(bufr_id)

      call codes_bufr_new_from_file(file_id, bufr_id, ret)
    end do

    call codes_close_file(file_id)

  end subroutine amdar_bufr_read

end module amdar_bufr_mod
