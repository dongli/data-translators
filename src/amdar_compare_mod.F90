module amdar_compare_mod

  use hash_table_mod
  use linked_list_mod
  use datetime
  use amdar_mod
  use utils_mod

  implicit none

  private

  public amdar_compare_run

  real, parameter :: diff_pressure_threshold          = 100.0 ! Pa
  real, parameter :: diff_height_threshold            = 10.0  ! m
  real, parameter :: diff_temperature_threshold       = 0.2   ! degC
  real, parameter :: diff_specific_humidity_threshold = 10.0  ! Mg/Kg
  real, parameter :: diff_wind_threshold              = 0.5   ! m/s

  type amdar_compare_type
    character(30) flight_name
    real :: lon                     = real_missing_value
    real :: lat                     = real_missing_value
    real :: height                  = real_missing_value
    real :: diff_pressure           = real_missing_value
    real :: diff_height             = real_missing_value
    real :: diff_temperature        = real_missing_value
    real :: diff_specific_humidity  = real_missing_value
    real :: diff_wind_u             = real_missing_value
    real :: diff_wind_v             = real_missing_value
    logical :: matched              = .true.
  end type amdar_compare_type

contains

  logical function is_domestic_flight(flight_name)

    character(*), intent(in) :: flight_name

    is_domestic_flight = (len_trim(flight_name) == 6 .and. flight_name(1:2) == 'CN')

  end function is_domestic_flight

  subroutine amdar_compare_run(flights1, records1, flights2, records2, output_file_path)

    type(hash_table_type), intent(in) :: flights1, flights2
    type(linked_list_type), intent(in) :: records1, records2
    character(*), intent(inout) :: output_file_path

    integer :: half_missing_value_count = 0
    integer :: missing_value_count = 0
    integer :: match_count = 0
    type(timedelta_type) dt
    type(hash_table_iterator_type) flight_iterator
    type(linked_list_iterator_type) record_iterator1, record_iterator2
    type(amdar_compare_type) compare
    type(linked_list_type) compares

    integer :: m = 0, n = 0

    flight_iterator = hash_table_iterator(flights1)
    do while (.not. flight_iterator%ended())
      select type (flight1 => flight_iterator%value)
      type is (amdar_flight_type)
        m = m + flight1%records%size
        ! Filter domestic or foreign flights.
        ! if (is_domestic_flight(flight1%name)) then
        !   call flight_iterator%next()
        !   cycle
        ! end if
        if (flights2%hashed(flight1%name)) then
          select type (flight2 => flights2%value(flight1%name))
          type is (amdar_flight_type)
            n = n + flight2%records%size
            record_iterator1 = linked_list_iterator(flight1%records)
            do while (.not. record_iterator1%ended())
              select type (record1 => record_iterator1%value)
              type is (amdar_record_type)
                record_iterator2 = linked_list_iterator(flight2%records)
                do while (.not. record_iterator2%ended())
                  select type (record2 => record_iterator2%value)
                  type is (amdar_record_type)
                    dt = record1%time - record2%time
                    if (abs(dt%total_minutes()) < 2) then
                      if (abs(record1%lon - record2%lon) < 0.01 .and. abs(record1%lat - record2%lat) < 0.01 .and. abs(record1%amdar_height - record2%amdar_height) < 10.0) then
                        compare%flight_name             = flight1%name
                        compare%lon                     = record1%lon
                        compare%lat                     = record1%lat
                        compare%height                  = record1%amdar_height
                        compare%diff_pressure           = subtract(record1%amdar_pressure,          record2%amdar_pressure)
                        compare%diff_height             = subtract(record1%amdar_height,            record2%amdar_height)
                        compare%diff_temperature        = subtract(record1%amdar_temperature,       record2%amdar_temperature)
                        compare%diff_specific_humidity  = subtract(record1%amdar_specific_humidity, record2%amdar_specific_humidity)
                        compare%diff_wind_u             = subtract(record1%amdar_wind_u,            record2%amdar_wind_u)
                        compare%diff_wind_v             = subtract(record1%amdar_wind_v,            record2%amdar_wind_v)
                        if ((is_missing(record1%amdar_pressure)          .and. .not. is_missing(record2%amdar_pressure))          .or. &
                            (is_missing(record1%amdar_height)            .and. .not. is_missing(record2%amdar_height))            .or. &
                            (is_missing(record1%amdar_temperature)       .and. .not. is_missing(record2%amdar_temperature))       .or. &
                            (is_missing(record1%amdar_specific_humidity) .and. .not. is_missing(record2%amdar_specific_humidity)) .or. &
                            (is_missing(record1%amdar_wind_u)            .and. .not. is_missing(record2%amdar_wind_u))            .or. &
                            (is_missing(record1%amdar_wind_v)            .and. .not. is_missing(record2%amdar_wind_v))) then
                          half_missing_value_count = half_missing_value_count + 1
                        end if
                        if (is_missing(compare%diff_pressure)          .or. &
                            is_missing(compare%diff_height)            .or. &
                            is_missing(compare%diff_temperature)       .or. &
                            is_missing(compare%diff_specific_humidity) .or. &
                            is_missing(compare%diff_wind_u)            .or. &
                            is_missing(compare%diff_wind_v)) then
                          missing_value_count = missing_value_count + 1
                        end if
                        if ((.not. is_missing(compare%diff_pressure)          .and. abs(compare%diff_pressure)          > diff_pressure_threshold)          .or. &
                            (.not. is_missing(compare%diff_height)            .and. abs(compare%diff_height)            > diff_height_threshold)            .or. &
                            (.not. is_missing(compare%diff_temperature)       .and. abs(compare%diff_temperature)       > diff_temperature_threshold)       .or. &
                            (.not. is_missing(compare%diff_specific_humidity) .and. abs(compare%diff_specific_humidity) > diff_specific_humidity_threshold) .or. &
                            (.not. is_missing(compare%diff_wind_u)            .and. abs(compare%diff_wind_u)            > diff_wind_threshold)              .or. &
                            (.not. is_missing(compare%diff_wind_v)            .and. abs(compare%diff_wind_v)            > diff_wind_threshold)) then
                          ! print *, compare%diff_pressure, compare%diff_height, compare%diff_temperature, compare%diff_specific_humidity, compare%diff_wind_u, compare%diff_wind_v
                          compare%matched = .false.
                        else
                          match_count = match_count + 1
                        end if
                        call compares%insert(trim(flight1%name) // '@' // record1%time%isoformat(), compare)
                      end if
                    end if
                  end select
                  call record_iterator2%next()
                end do
              end select
              call record_iterator1%next()
            end do
          end select
        end if
      end select
      call flight_iterator%next()
    end do

    print *, m, n
    write(*, *) '==> Intersected records count: ', trim(to_string(compares%size))
    write(*, *) '==> Matched records count:     ', trim(to_string(match_count)), ' (' // to_string(100 * real(match_count) / real(compares%size)) // '%)'
    write(*, *) '==> Half missing value count:  ', trim(to_string(half_missing_value_count)), ' (' // to_string(100 * real(half_missing_value_count) / real(compares%size)) // '%)'

    ! Output compare results.
    call amdar_compare_output(compares, output_file_path)

  end subroutine amdar_compare_run

  subroutine amdar_compare_output(compares, output_file_path)

    use netcdf

    type(linked_list_type), intent(in) :: compares
    character(*), intent(inout) :: output_file_path

    integer ncid, ierr, i
    integer record_dimid
    integer flight_name_varid
    integer lon_varid
    integer lat_varid
    integer height_varid
    integer diff_temperature_varid
    integer diff_pressure_varid
    integer diff_height_varid
    integer diff_specific_humidity_varid
    integer diff_wind_u_varid
    integer diff_wind_v_varid

    character(30), allocatable :: flight_names(:)
    real, allocatable :: lon(:)
    real, allocatable :: lat(:)
    real, allocatable :: height(:)
    real, allocatable :: diff_temperature(:)
    real, allocatable :: diff_pressure(:)
    real, allocatable :: diff_height(:)
    real, allocatable :: diff_specific_humidity(:)
    real, allocatable :: diff_wind_u(:)
    real, allocatable :: diff_wind_v(:)

    type(linked_list_iterator_type) compare_iterator

    allocate(flight_names(compares%size))
    allocate(lon(compares%size))
    allocate(lat(compares%size))
    allocate(height(compares%size))
    allocate(diff_temperature(compares%size))
    allocate(diff_pressure(compares%size))
    allocate(diff_height(compares%size))
    allocate(diff_specific_humidity(compares%size))
    allocate(diff_wind_u(compares%size))
    allocate(diff_wind_v(compares%size))

    compare_iterator = linked_list_iterator(compares)
    i = 1
    do while (.not. compare_iterator%ended())
      select type (compare => compare_iterator%value)
      type is (amdar_compare_type)
        flight_names(i) = compare%flight_name
        lon(i) = compare%lon
        lat(i) = compare%lat
        diff_temperature(i) = compare%diff_temperature
        diff_pressure(i) = compare%diff_pressure
        diff_height(i) = compare%diff_height
        diff_specific_humidity(i) = compare%diff_specific_humidity
        diff_wind_u(i) = compare%diff_wind_u
        diff_wind_v(i) = compare%diff_wind_v
      end select
      i = i + 1
      call compare_iterator%next()
    end do

    if (output_file_path == '') output_file_path = 'compares.nc'

    ierr = nf90_create(output_file_path, NF90_CLOBBER, ncid)

    ierr = nf90_def_dim(ncid, 'record', compares%size, record_dimid)

    ierr = nf90_def_var(ncid, 'flight_name', NF90_STRING, [record_dimid], flight_name_varid)

    ierr = nf90_def_var(ncid, 'lon', NF90_FLOAT, [record_dimid], lon_varid)

    ierr = nf90_def_var(ncid, 'lat', NF90_FLOAT, [record_dimid], lat_varid)

    ierr = nf90_def_var(ncid, 'height', NF90_FLOAT, [record_dimid], height_varid)

    ierr = nf90_def_var(ncid, 'diff_pressure', NF90_FLOAT, [record_dimid], diff_pressure_varid)

    ierr = nf90_put_att(ncid, diff_pressure_varid, '_FillValue', real_missing_value)

    ierr = nf90_def_var(ncid, 'diff_height', NF90_FLOAT, [record_dimid], diff_height_varid)

    ierr = nf90_put_att(ncid, diff_height_varid, '_FillValue', real_missing_value)

    ierr = nf90_def_var(ncid, 'diff_temperature', NF90_FLOAT, [record_dimid], diff_temperature_varid)

    ierr = nf90_put_att(ncid, diff_temperature_varid, '_FillValue', real_missing_value)

    ierr = nf90_def_var(ncid, 'diff_specific_humidity', NF90_FLOAT, [record_dimid], diff_specific_humidity_varid)

    ierr = nf90_put_att(ncid, diff_specific_humidity_varid, '_FillValue', real_missing_value)

    ierr = nf90_def_var(ncid, 'diff_wind_u', NF90_FLOAT, [record_dimid], diff_wind_u_varid)

    ierr = nf90_put_att(ncid, diff_wind_u_varid, '_FillValue', real_missing_value)

    ierr = nf90_def_var(ncid, 'diff_wind_v', NF90_FLOAT, [record_dimid], diff_wind_v_varid)

    ierr = nf90_put_att(ncid, diff_wind_v_varid, '_FillValue', real_missing_value)

    ierr = nf90_enddef(ncid)

    ierr = nf90_put_var(ncid, flight_name_varid, flight_names)

    ierr = nf90_put_var(ncid, lon_varid, lon)

    ierr = nf90_put_var(ncid, lat_varid, lat)

    ierr = nf90_put_var(ncid, height_varid, height)

    ierr = nf90_put_var(ncid, diff_pressure_varid, diff_pressure)

    ierr = nf90_put_var(ncid, diff_height_varid, diff_height)

    ierr = nf90_put_var(ncid, diff_temperature_varid, diff_temperature)

    ierr = nf90_put_var(ncid, diff_specific_humidity_varid, diff_specific_humidity)

    ierr = nf90_put_var(ncid, diff_wind_u_varid, diff_wind_u)

    ierr = nf90_put_var(ncid, diff_wind_v_varid, diff_wind_v)

    ierr = nf90_close(ncid)

    deallocate(diff_temperature)
    deallocate(diff_pressure)
    deallocate(diff_height)
    deallocate(diff_specific_humidity)
    deallocate(diff_wind_u)
    deallocate(diff_wind_v)

  end subroutine amdar_compare_output

end module amdar_compare_mod