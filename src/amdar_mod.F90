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
    ! p is in obs_site_base_type
    real    :: h    = real_missing_value ! Height (m)
    real    :: ta   = real_missing_value ! Temperature (degC)
    real    :: sh   = real_missing_value ! Specific humidity (Mg/Kg)
    real    :: td   = real_missing_value ! Dewpoint temperature (degC)
    real    :: rh   = real_missing_value ! Relative humidity (%)
    real    :: ws   = real_missing_value ! Wind speed (m/s)
    real    :: wd   = real_missing_value ! Wind direction (deg)
    real    :: ua   = real_missing_value ! U wind component (m/s)
    real    :: va   = real_missing_value ! V wind component (m/s)
    integer :: trb  = int_missing_value  ! Turbulence index

    integer :: p_qc  = int_missing_value
    integer :: h_qc  = int_missing_value
    integer :: ta_qc = int_missing_value
    integer :: td_qc = int_missing_value
    integer :: sh_qc = int_missing_value
    integer :: rh_qc = int_missing_value
    integer :: wd_qc = int_missing_value
    integer :: ws_qc = int_missing_value
    integer :: ua_qc = int_missing_value
    integer :: va_qc = int_missing_value

    real :: p_cr  = real_missing_value
    real :: h_cr  = real_missing_value
    real :: ta_cr = real_missing_value
    real :: sh_cr = real_missing_value
    real :: ua_cr = real_missing_value
    real :: va_cr = real_missing_value
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
    if (is_missing(this%ta)) then
      write(*, *) '  VALUE: X'
    else
      write(*, *) '  VALUE: ', this%ta
    end if
    if (is_missing(this%ta_cr)) then
      write(*, *) '  CORRECTION: X'
    else
      write(*, *) '  CORRECTION: ', this%ta_cr
    end if
    write(*, *) 'SPECIFIC HUMIDITY: '
    if (is_missing(this%sh)) then
      write(*, *) '  VALUE: X'
    else
      write(*, *) '  VALUE: ', this%sh
    end if
    if (is_missing(this%sh_cr)) then
      write(*, *) '  CORRECTION: X'
    else
      write(*, *) '  CORRECTION: ', this%sh_cr
    end if
    write(*, *) 'DEWPOINT: '
    if (is_missing(this%td)) then
      write(*, *) '  VALUE: X'
    else
      write(*, *) '  VALUE: ', this%td
    end if
    write(*, *) 'PRESSURE: '
    if (is_missing(this%p)) then
      write(*, *) '  VALUE: X'
    else
      write(*, *) '  VALUE: ', this%p
    end if
    if (is_missing(this%p_cr)) then
      write(*, *) '  CORRECTION: X'
    else
      write(*, *) '  CORRECTION: ', this%p_cr
    end if
    write(*, *) 'HEIGHT: '
    if (is_missing(this%h)) then
      write(*, *) '  VALUE: X'
    else
      write(*, *) '  VALUE: ', this%h
    end if
    if (is_missing(this%h_cr)) then
      write(*, *) '  CORRECTION: X'
    else
      write(*, *) '  CORRECTION: ', this%h_cr
    end if
    write(*, *) 'WIND U: '
    if (is_missing(this%ua)) then
      write(*, *) '  VALUE: X'
    else
      write(*, '(A, F8.1)') '  VALUE: ', this%ua
    end if
    if (is_missing(this%ua_cr)) then
      write(*, *) '  CORRECTION: X'
    else
      write(*, *) '  CORRECTION: ', this%ua_cr
    end if
    write(*, *) 'WIND V: '
    if (is_missing(this%va)) then
      write(*, *) '  VALUE: X'
    else
      write(*, '(A, F8.1)') '  VALUE: ', this%va
    end if
    if (is_missing(this%va_cr)) then
      write(*, *) '  CORRECTION: X'
    else
      write(*, *) '  CORRECTION: ', this%va_cr
    end if

  end subroutine amdar_record_print

end module amdar_mod
