module radar_mod

  use obs_base_mod

  implicit none

  type, extends(obs_station_type) :: radar_station_type
    character(10) id
    real freq
  contains
    procedure :: print => radar_station_print
  end type radar_station_type

  type radar_radial_type
    integer radial_id
    integer elevation_id
    real azimuth
    real elevation
    integer num_datum
    real, allocatable :: data(:)
  end type radar_radial_type

  type radar_moment_type
    character(10) type
    integer num_radial
    type(radar_radial_type), allocatable :: radials(:)
  end type radar_moment_type

  type radar_tilt_type
    real elevation
    integer num_moment
    type(radar_moment_type), allocatable :: moments(:)
  end type radar_tilt_type

  type, extends(obs_static_record_base_type) :: radar_record_type
    integer vcp ! Volume Coverage Pattern
    integer num_tilt
    type(radar_tilt_type), allocatable :: tilts(:)
  end type radar_record_type

contains

  subroutine radar_station_print(this)

    class(radar_station_type), intent(in) :: this

    write(*, *) this%id
    write(*, *) 'lon: ', this%lon
    write(*, *) 'lat: ', this%lat
    write(*, *) 'z:   ', this%z
    write(*, *) 'freq:', this%freq

  end subroutine radar_station_print

end module radar_mod