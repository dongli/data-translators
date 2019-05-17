module synop_mod

  use obs_base_mod
  use params_mod
  use hash_table_mod
  use linked_list_mod

  implicit none

  type, extends(obs_station_type) :: synop_station_type
    type(linked_list_type), pointer :: records => null()
  contains
    procedure :: init => synop_station_init
    final :: synop_station_final
  end type synop_station_type

  type, extends(obs_static_record_base_type) :: synop_record_type
    type(synop_station_type), pointer :: station
    real :: temperature       = real_missing_value ! Temperature (degC)
    real :: dewpoint          = real_missing_value ! Dewpoint temperature (degC)
    real :: pressure          = real_missing_value ! Surface pressure (Pa)
    real :: relative_humidity = real_missing_value ! Relative humidity (%)
    real :: specific_humidity = real_missing_value ! Specific humidity (Mg/Kg)
    real :: wind_speed        = real_missing_value ! Wind speed (m/s)
    real :: wind_direction    = real_missing_value ! Wind direction (deg)
    real :: wind_u            = real_missing_value ! U wind component (m/s)
    real :: wind_v            = real_missing_value ! V wind component (m/s)
    real :: rain_01h          = real_missing_value ! 1h accumulated total precipitation (mm)
    real :: rain_03h          = real_missing_value ! 3h accumulated total precipitation (mm)
    real :: rain_06h          = real_missing_value ! 6h accumulated total precipitation (mm)
    real :: rain_12h          = real_missing_value ! 12h accumulated total precipitation (mm)
    real :: rain_24h          = real_missing_value ! 24h accumulated total precipitation (mm)
    real :: cloud_amount      = real_missing_value ! Cloud amount (???)

    integer :: type                 = int_missing_value
    integer :: temperature_qc       = int_missing_value
    integer :: dewpoint_qc          = int_missing_value
    integer :: pressure_qc          = int_missing_value
    integer :: relative_humidity_qc = int_missing_value
    integer :: specific_humidity_qc = int_missing_value
    integer :: wind_qc              = int_missing_value

    real :: temperature_correct       = real_missing_value
    real :: specific_humidity_correct = real_missing_value
    real :: pressure_correct          = real_missing_value
    real :: wind_u_correct            = real_missing_value
    real :: wind_v_correct            = real_missing_value
    real :: wind_direction_correct    = real_missing_value
    real :: wind_speed_correct        = real_missing_value
  contains
    procedure :: print => synop_record_print
  end type synop_record_type

contains

  subroutine synop_station_init(this, name, lon, lat, z)

    class(synop_station_type), intent(inout) :: this
    character(*), intent(in) :: name
    real, intent(in) :: lon
    real, intent(in) :: lat
    real, intent(in) :: z

    this%name = name
    this%lon = lon
    this%lat = lat
    this%z = z
    if (.not. associated(this%records)) allocate(this%records)

  end subroutine synop_station_init

  subroutine synop_station_final(this)

    type(synop_station_type), intent(inout) :: this

    if (associated(this%records)) deallocate(this%records)

  end subroutine synop_station_final

  subroutine synop_record_print(this)

    class(synop_record_type), intent(in) :: this

    integer i

    write(*, *) '--'
    write(*, *) 'STATION NAME: ', trim(this%station%name)
    write(*, *) 'OBS TIME: ', trim(this%time%isoformat())
    write(*, *) 'LON:', this%station%lon, 'LAT:', this%station%lat, 'Z:', this%station%z
    write(*, *) 'TEMPERATURE: '
    if (is_missing(this%temperature)) then
      write(*, *) '  VALUE: X'
    else
      write(*, '(A, F8.2)', advance='no') '  VALUE: ', this%temperature
      if (is_missing(this%temperature_correct)) then
        write(*, *)
      else
        write(*, '(" (", F8.2, ")")') this%temperature_correct
      end if
    end if
    write(*, *) 'SPECIFIC HUMIDITY: '
    if (is_missing(this%specific_humidity)) then
      write(*, *) '  VALUE: X'
    else
      write(*, '(A, F8.2)', advance='no') '  VALUE: ', this%specific_humidity
      if (is_missing(this%specific_humidity_correct)) then
        write(*, *)
      else
        write(*, '(" (", F8.2, ")")') this%specific_humidity_correct
      end if
    end if
    write(*, *) 'DEWPOINT: '
    if (is_missing(this%dewpoint)) then
      write(*, *) '  VALUE: X'
    else
      write(*, *) '  VALUE: ', this%dewpoint
    end if
    write(*, *) 'PRESSURE: '
    if (is_missing(this%pressure)) then
      write(*, *) '  VALUE: X'
    else
      write(*, '(A, F10.2)', advance='no') '  VALUE: ', this%pressure
      if (is_missing(this%pressure_correct)) then
        write(*, *)
      else
        write(*, '(" (", F8.2, ")")') this%pressure_correct
      end if
    end if
    write(*, *) 'WIND U: '
    if (is_missing(this%wind_u)) then
      write(*, *) '  VALUE: X'
    else
      write(*, '(A, F8.1)', advance='no') '  VALUE: ', this%wind_u
      if (is_missing(this%wind_u_correct)) then
        write(*, *)
      else
        write(*, '(" (", F8.2, ")")') this%wind_u_correct
      end if
    end if
    write(*, *) 'WIND V: '
    if (is_missing(this%wind_v)) then
      write(*, *) '  VALUE: X'
    else
      write(*, '(A, F8.1)', advance='no') '  VALUE: ', this%wind_v
      if (is_missing(this%wind_v_correct)) then
        write(*, *)
      else
        write(*, '(" (", F8.2, ")")') this%wind_v_correct
      end if
    end if
    write(*, *) 'RAIN: ', this%rain_01h, this%rain_03h, this%rain_06h, this%rain_12h, this%rain_24h

  end subroutine synop_record_print

end module synop_mod
