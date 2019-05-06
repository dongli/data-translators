module amdar_mod

  use obs_base_mod
  use params_mod
  use hash_table_mod
  use linked_list_mod

  implicit none

  type, extends(obs_site_nopos_base_type) :: amdar_flight_type
    character(8) :: number = ''
    type(linked_list_type), pointer :: records => null()
  contains
    procedure :: init => amdar_flight_init
    final :: amdar_flight_final
  end type amdar_flight_type

  integer, parameter :: max_stack = 10

  type, extends(obs_drift_record_base_type) :: amdar_record_type
    type(amdar_flight_type), pointer :: flight
    real    :: pressure          = real_missing_value ! Pressure (Pa)
    real    :: height            = real_missing_value ! Height (m)
    real    :: temperature       = real_missing_value ! Temperature (degC)
    real    :: specific_humidity = real_missing_value ! Specific humidity (Mg/Kg)
    real    :: dewpoint          = real_missing_value ! Dewpoint temperature (degC)
    real    :: relative_humidity = real_missing_value ! Relative humidity (%)
    real    :: wind_speed        = real_missing_value ! Wind speed (m/s)
    real    :: wind_direction    = real_missing_value ! Wind direction (deg)
    real    :: wind_u            = real_missing_value ! U wind component (m/s)
    real    :: wind_v            = real_missing_value ! V wind component (m/s)
    integer :: turbulence_index  = int_missing_value  ! Turbulence index

    integer :: pressure_qc          = int_missing_value
    integer :: height_qc            = int_missing_value
    integer :: temperature_qc       = int_missing_value
    integer :: dewpoint_qc          = int_missing_value
    integer :: specific_humidity_qc = int_missing_value
    integer :: relative_humidity_qc = int_missing_value
    integer :: wind_qc              = int_missing_value

    real :: pressure_correct          = real_missing_value
    real :: height_correct            = real_missing_value
    real :: temperature_correct       = real_missing_value
    real :: specific_humidity_correct = real_missing_value
    real :: wind_u_correct            = real_missing_value
    real :: wind_v_correct            = real_missing_value
  contains
    procedure :: print => amdar_record_print
  end type amdar_record_type

contains

  subroutine amdar_flight_init(this, name)

    class(amdar_flight_type), intent(inout) :: this
    character(*), intent(in) :: name

    this%name = name
    if (.not. associated(this%records)) allocate(this%records)

  end subroutine amdar_flight_init

  subroutine amdar_flight_final(this)

    type(amdar_flight_type), intent(inout) :: this

    if (associated(this%records)) deallocate(this%records)

  end subroutine amdar_flight_final

  subroutine amdar_record_print(this)

    class(amdar_record_type), intent(in) :: this

    integer i

    write(*, *) '--'
    write(*, *) 'PLATFORM TYPE: ', trim(this%platform_type)
    write(*, *) 'FLIGHT NAME: ', trim(this%flight%name)
    write(*, *) 'OBS TIME: ', trim(this%time%isoformat())
    write(*, *) 'LON:', this%lon, 'LAT:', this%lat
    write(*, *) 'TEMPERATURE:'
    if (is_missing(this%temperature)) then
      write(*, *) '  VALUE: X'
    else
      write(*, *) '  VALUE: ', this%temperature
    end if
    if (is_missing(this%temperature_correct)) then
      write(*, *) '  CORRECTION: X'
    else
      write(*, *) '  CORRECTION: ', this%temperature_correct
    end if
    write(*, *) 'SPECIFIC HUMIDITY: '
    if (is_missing(this%specific_humidity)) then
      write(*, *) '  VALUE: X'
    else
      write(*, *) '  VALUE: ', this%specific_humidity
    end if
    if (is_missing(this%specific_humidity_correct)) then
      write(*, *) '  CORRECTION: X'
    else
      write(*, *) '  CORRECTION: ', this%specific_humidity_correct
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
      write(*, *) '  VALUE: ', this%pressure
    end if
    if (is_missing(this%pressure_correct)) then
      write(*, *) '  CORRECTION: X'
    else
      write(*, *) '  CORRECTION: ', this%pressure_correct
    end if
    write(*, *) 'HEIGHT: '
    if (is_missing(this%height)) then
      write(*, *) '  VALUE: X'
    else
      write(*, *) '  VALUE: ', this%height
    end if
    if (is_missing(this%height_correct)) then
      write(*, *) '  CORRECTION: X'
    else
      write(*, *) '  CORRECTION: ', this%height_correct
    end if
    write(*, *) 'WIND U: '
    if (is_missing(this%wind_u)) then
      write(*, *) '  VALUE: X'
    else
      write(*, '(A, F8.1)') '  VALUE: ', this%wind_u
    end if
    if (is_missing(this%wind_u_correct)) then
      write(*, *) '  CORRECTION: X'
    else
      write(*, *) '  CORRECTION: ', this%wind_u_correct
    end if
    write(*, *) 'WIND V: '
    if (is_missing(this%wind_v)) then
      write(*, *) '  VALUE: X'
    else
      write(*, '(A, F8.1)') '  VALUE: ', this%wind_v
    end if
    if (is_missing(this%wind_v_correct)) then
      write(*, *) '  CORRECTION: X'
    else
      write(*, *) '  CORRECTION: ', this%wind_v_correct
    end if

  end subroutine amdar_record_print

end module amdar_mod
