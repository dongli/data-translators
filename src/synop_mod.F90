module synop_mod

  use container
  use obs_base_mod
  use params_mod

  implicit none

  type, extends(obs_station_type) :: synop_station_type
    type(linked_list_type), pointer :: records => null()
  contains
    procedure :: init => synop_station_init
    final :: synop_station_final
  end type synop_station_type

  type, extends(obs_static_record_base_type) :: synop_record_type
    type(synop_station_type), pointer :: station
    real :: ta   = real_missing_value ! Temperature (degC)
    real :: td   = real_missing_value ! Dewpoint temperature (degC)
    real :: p    = real_missing_value ! Surface pressure (hPa)
    real :: rh   = real_missing_value ! Relative humidity (%)
    real :: sh   = real_missing_value ! Specific humidity (Mg/Kg)
    real :: ws   = real_missing_value ! Wind speed (m/s)
    real :: wd   = real_missing_value ! Wind direction (deg)
    real :: ua   = real_missing_value ! U wind component (m/s)
    real :: va   = real_missing_value ! V wind component (m/s)
    real :: r01h = real_missing_value ! 1h accumulated total precipitation (mm)
    real :: r03h = real_missing_value ! 3h accumulated total precipitation (mm)
    real :: r06h = real_missing_value ! 6h accumulated total precipitation (mm)
    real :: r12h = real_missing_value ! 12h accumulated total precipitation (mm)
    real :: r24h = real_missing_value ! 24h accumulated total precipitation (mm)
    real :: clc  = real_missing_value ! Cloud amount (???)
    real :: vis  = real_missing_value ! Visibility (km)

    integer :: type  = int_missing_value
    integer :: ta_qc = int_missing_value
    integer :: td_qc = int_missing_value
    integer :: p_qc  = int_missing_value
    integer :: rh_qc = int_missing_value
    integer :: sh_qc = int_missing_value
    integer :: ws_qc = int_missing_value
    integer :: wd_qc = int_missing_value
    integer :: ua_qc = int_missing_value
    integer :: va_qc = int_missing_value

    real :: ta_cr = real_missing_value
    real :: sh_cr = real_missing_value
    real :: p_cr  = real_missing_value
    real :: ws_cr = real_missing_value
    real :: wd_cr = real_missing_value
    real :: ua_cr = real_missing_value
    real :: va_cr = real_missing_value
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
    if (is_missing(this%ta)) then
      write(*, *) '  VALUE: X'
    else
      write(*, '(A, F8.2)', advance='no') '  VALUE: ', this%ta
      if (is_missing(this%ta_cr)) then
        write(*, *)
      else
        write(*, '(" (", F8.2, ")")') this%ta_cr
      end if
    end if
    write(*, *) 'SPECIFIC HUMIDITY: '
    if (is_missing(this%sh)) then
      write(*, *) '  VALUE: X'
    else
      write(*, '(A, F8.2)', advance='no') '  VALUE: ', this%sh
      if (is_missing(this%sh_cr)) then
        write(*, *)
      else
        write(*, '(" (", F8.2, ")")') this%sh_cr
      end if
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
      write(*, '(A, F10.2)', advance='no') '  VALUE: ', this%p
      if (is_missing(this%p_cr)) then
        write(*, *)
      else
        write(*, '(" (", F8.2, ")")') this%p_cr
      end if
    end if
    write(*, *) 'WIND U: '
    if (is_missing(this%ua)) then
      write(*, *) '  VALUE: X'
    else
      write(*, '(A, F8.1)', advance='no') '  VALUE: ', this%ua
      if (is_missing(this%ua_cr)) then
        write(*, *)
      else
        write(*, '(" (", F8.2, ")")') this%ua_cr
      end if
    end if
    write(*, *) 'WIND V: '
    if (is_missing(this%va)) then
      write(*, *) '  VALUE: X'
    else
      write(*, '(A, F8.1)', advance='no') '  VALUE: ', this%va
      if (is_missing(this%va_cr)) then
        write(*, *)
      else
        write(*, '(" (", F8.2, ")")') this%va_cr
      end if
    end if
    write(*, *) 'RAIN: ', this%r01h, this%r03h, this%r06h, this%r12h, this%r24h

  end subroutine synop_record_print

end module synop_mod
