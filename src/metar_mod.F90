module metar_mod

  use obs_base_mod
  use params_mod
  use hash_table_mod
  use linked_list_mod

  implicit none

  type, extends(obs_station_type) :: metar_station_type
    type(linked_list_type), pointer :: records => null()
  contains
    procedure :: init => metar_station_init
    final :: metar_station_final
  end type metar_station_type

  integer, parameter :: max_stack = 255

  type, extends(obs_static_record_base_type) :: metar_record_type
    type(metar_station_type), pointer :: station
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

    integer :: type                     = int_missing_value
    integer :: temperature_qc       = int_missing_value
    integer :: dewpoint_qc          = int_missing_value
    integer :: pressure_qc          = int_missing_value
    integer :: relative_humidity_qc = int_missing_value
    integer :: specific_humidity_qc = int_missing_value
    integer :: wind_qc              = int_missing_value

    ! For PrepBUFR data
    real :: temperature_stack(max_stack) = real_missing_value
    integer :: temperature_stack_qc(max_stack) = int_missing_value
    integer :: temperature_stack_pc(max_stack) = int_missing_value

    real :: specific_humidity_stack(max_stack) = real_missing_value
    integer :: specific_humidity_stack_qc(max_stack) = int_missing_value
    integer :: specific_humidity_stack_pc(max_stack) = int_missing_value

    real :: pressure_stack(max_stack) = real_missing_value
    integer :: pressure_stack_qc(max_stack) = int_missing_value
    integer :: pressure_stack_pc(max_stack) = int_missing_value

    real :: wind_u_stack(max_stack) = real_missing_value
    real :: wind_v_stack(max_stack) = real_missing_value
    integer :: wind_stack_qc(max_stack) = int_missing_value
    integer :: wind_stack_pc(max_stack) = int_missing_value
  contains
    procedure :: print => metar_record_print
  end type metar_record_type

contains

  subroutine metar_station_init(this, name, lon, lat, z)

    class(metar_station_type), intent(inout) :: this
    character(*), intent(in) :: name
    real, intent(in) :: lon
    real, intent(in) :: lat
    real, intent(in) :: z

    this%name = name
    this%lon = lon
    this%lat = lat
    this%z = z
    if (.not. associated(this%records)) allocate(this%records)

  end subroutine metar_station_init

  subroutine metar_station_final(this)

    type(metar_station_type), intent(inout) :: this

    if (associated(this%records)) deallocate(this%records)

  end subroutine metar_station_final

  subroutine metar_record_print(this)

    class(metar_record_type), intent(in) :: this

    integer i

    print *, '--'
    print *, 'STATION NAME: ', trim(this%station%name)
    print *, 'OBS TIME: ', trim(this%time%isoformat())
    print *, 'LON:', this%station%lon, 'LAT:', this%station%lat, 'Z:', this%station%z
    write(*, *) 'TEMPERATURE: '
    if (is_missing(this%temperature)) then
      write(*, *) '  VALUE: X'
    else
      write(*, *) '  VALUE: ', this%temperature
    end if
    write(*, '(A)', advance='no') '   VALUE STACK: '
    do i = 1, 4
      if (is_missing(this%temperature_stack(i))) then
        write(*, '(A)', advance='no') 'X, '
      else
        write(*, '(F6.1, ", ")', advance='no') this%temperature_stack(i)
      end if
    end do
    write(*, *)
    if (is_missing(this%temperature_qc)) then
      write(*, *) '  QC: X'
    else
      write(*, *) '  QC: ', this%temperature_qc
    end if
    write(*, '(A)', advance='no') '   QC STACK: '
    do i = 1, 4
      if (is_missing(this%temperature_stack_qc(i))) then
        write(*, '(A)', advance='no') 'X, '
      else
        write(*, '(I3, ", ")', advance='no') this%temperature_stack_qc(i)
      end if
    end do
    write(*, *)
    write(*, '(A)', advance='no') '   PC STACK: '
    do i = 1, 4
      if (is_missing(this%temperature_stack_pc(i))) then
        write(*, '(A)', advance='no') 'X, '
      else
        write(*, '(I3, ", ")', advance='no') this%temperature_stack_pc(i)
      end if
    end do
    write(*, *)
    write(*, *) 'SPECIFIC HUMIDITY: '
    if (is_missing(this%specific_humidity)) then
      write(*, *) '  VALUE: X'
    else
      write(*, *) '  VALUE: ', this%specific_humidity
    end if
    write(*, '(A)', advance='no') '   VALUE STACK: '
    do i = 1, 4
      if (is_missing(this%specific_humidity_stack(i))) then
        write(*, '(A)', advance='no') 'X, '
      else
        write(*, '(F10.2, ", ")', advance='no') this%specific_humidity_stack(i)
      end if
    end do
    write(*, *)
    if (is_missing(this%specific_humidity_qc)) then
      write(*, *) '  QC: X'
    else
      write(*, *) '  QC: ', this%specific_humidity_qc
    end if
    write(*, '(A)', advance='no') '   QC STACK: '
    do i = 1, 4
      if (is_missing(this%specific_humidity_stack_qc(i))) then
        write(*, '(A)', advance='no') 'X, '
      else
        write(*, '(I3, ", ")', advance='no') this%specific_humidity_stack_qc(i)
      end if
    end do
    write(*, *)
    write(*, '(A)', advance='no') '   PC STACK: '
    do i = 1, 4
      if (is_missing(this%specific_humidity_stack_pc(i))) then
        write(*, '(A)', advance='no') 'X, '
      else
        write(*, '(I3, ", ")', advance='no') this%specific_humidity_stack_pc(i)
      end if
    end do
    write(*, *)
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
    write(*, '(A)', advance='no') '   VALUE STACK: '
    do i = 1, 4
      if (is_missing(this%pressure_stack(i))) then
        write(*, '(A)', advance='no') 'X, '
      else
        write(*, '(F8.1, ", ")', advance='no') this%pressure_stack(i)
      end if
    end do
    write(*, *)
    if (is_missing(this%pressure_qc)) then
      write(*, *) '  QC: X'
    else
      write(*, *) '  QC: ', this%pressure_qc
    end if
    write(*, '(A)', advance='no') '   QC STACK: '
    do i = 1, 4
      if (is_missing(this%pressure_stack_qc(i))) then
        write(*, '(A)', advance='no') 'X, '
      else
        write(*, '(I3, ", ")', advance='no') this%pressure_stack_qc(i)
      end if
    end do
    write(*, *)
    write(*, '(A)', advance='no') '   PC STACK: '
    do i = 1, 4
      if (is_missing(this%pressure_stack_pc(i))) then
        write(*, '(A)', advance='no') 'X, '
      else
        write(*, '(I3, ", ")', advance='no') this%pressure_stack_pc(i)
      end if
    end do
    write(*, *)
    write(*, *) 'WIND U: '
    if (is_missing(this%wind_u)) then
      write(*, *) '  VALUE: X'
    else
      write(*, '(A, F8.1)') '  VALUE: ', this%wind_u
    end if
    write(*, '(A)', advance='no') '   VALUE STACK: '
    do i = 1, 4
      if (is_missing(this%wind_u_stack(i))) then
        write(*, '(A)', advance='no') 'X, '
      else
        write(*, '(F8.1, ", ")', advance='no') this%wind_u_stack(i)
      end if
    end do
    write(*, *)
    if (is_missing(this%wind_qc)) then
      write(*, *) '  QC: X'
    else
      write(*, '(A, I2)') '  QC: ', this%wind_qc
    end if
    write(*, '(A)', advance='no') '   QC STACK: '
    do i = 1, 4
      if (is_missing(this%wind_stack_qc(i))) then
        write(*, '(A)', advance='no') 'X, '
      else
        write(*, '(I3, ", ")', advance='no') this%wind_stack_qc(i)
      end if
    end do
    write(*, *)
    write(*, '(A)', advance='no') '   PC STACK: '
    do i = 1, 4
      if (is_missing(this%wind_stack_pc(i))) then
        write(*, '(A)', advance='no') 'X, '
      else
        write(*, '(I3, ", ")', advance='no') this%wind_stack_pc(i)
      end if
    end do
    write(*, *)
    write(*, *) 'WIND V: '
    if (is_missing(this%wind_v)) then
      write(*, *) '  VALUE: X'
    else
      write(*, '(A, F8.1)') '  VALUE: ', this%wind_v
    end if
    write(*, '(A)', advance='no') '   VALUE STACK: '
    do i = 1, 4
      if (is_missing(this%wind_v_stack(i))) then
        write(*, '(A)', advance='no') 'X, '
      else
        write(*, '(F8.1, ", ")', advance='no') this%wind_v_stack(i)
      end if
    end do
    write(*, *)
    if (is_missing(this%wind_qc)) then
      write(*, *) '  QC: X'
    else
      write(*, '(A, I2)') '  QC: ', this%wind_qc
    end if
    write(*, '(A)', advance='no') '   QC STACK: '
    do i = 1, 4
      if (is_missing(this%wind_stack_qc(i))) then
        write(*, '(A)', advance='no') 'X, '
      else
        write(*, '(I3, ", ")', advance='no') this%wind_stack_qc(i)
      end if
    end do
    write(*, *)
    write(*, '(A)', advance='no') '   PC STACK: '
    do i = 1, 4
      if (is_missing(this%wind_stack_pc(i))) then
        write(*, '(A)', advance='no') 'X, '
      else
        write(*, '(I3, ", ")', advance='no') this%wind_stack_pc(i)
      end if
    end do
    write(*, *)
    write(*, *) 'RAIN: ', this%rain_01h, this%rain_03h, this%rain_06h, this%rain_12h, this%rain_24h

  end subroutine metar_record_print

end module metar_mod
