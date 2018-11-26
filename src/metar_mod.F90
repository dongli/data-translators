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
    real :: sfc_temperature       = real_missing_value ! Temperature (degC)
    real :: sfc_dewpoint          = real_missing_value ! Dewpoint temperature (degC)
    real :: sfc_pressure          = real_missing_value ! Surface pressure (Pa)
    real :: sfc_relative_humidity = real_missing_value ! Relative humidity (%)
    real :: sfc_specific_humidity = real_missing_value ! Specific humidity (Mg/Kg)
    real :: sfc_wind_speed        = real_missing_value ! Wind speed (m/s)
    real :: sfc_wind_direction    = real_missing_value ! Wind direction (deg)
    real :: sfc_wind_u            = real_missing_value ! U wind component (m/s)
    real :: sfc_wind_v            = real_missing_value ! V wind component (m/s)
    real :: sfc_rain_01h          = real_missing_value ! 1h accumulated total precipitation (mm)
    real :: sfc_rain_03h          = real_missing_value ! 3h accumulated total precipitation (mm)
    real :: sfc_rain_06h          = real_missing_value ! 6h accumulated total precipitation (mm)
    real :: sfc_rain_12h          = real_missing_value ! 12h accumulated total precipitation (mm)
    real :: sfc_rain_24h          = real_missing_value ! 24h accumulated total precipitation (mm)
    real :: sfc_cloud_amount      = real_missing_value ! Cloud amount (???)

    integer :: type                     = int_missing_value
    integer :: sfc_temperature_qc       = int_missing_value
    integer :: sfc_dewpoint_qc          = int_missing_value
    integer :: sfc_pressure_qc          = int_missing_value
    integer :: sfc_relative_humidity_qc = int_missing_value
    integer :: sfc_specific_humidity_qc = int_missing_value
    integer :: sfc_wind_qc              = int_missing_value

    real :: sfc_temperature_stack(max_stack) = real_missing_value
    integer :: sfc_temperature_stack_qc(max_stack) = int_missing_value
    integer :: sfc_temperature_stack_pc(max_stack) = int_missing_value

    real :: sfc_specific_humidity_stack(max_stack) = real_missing_value
    integer :: sfc_specific_humidity_stack_qc(max_stack) = int_missing_value
    integer :: sfc_specific_humidity_stack_pc(max_stack) = int_missing_value

    real :: sfc_pressure_stack(max_stack) = real_missing_value
    integer :: sfc_pressure_stack_qc(max_stack) = int_missing_value
    integer :: sfc_pressure_stack_pc(max_stack) = int_missing_value

    real :: sfc_wind_u_stack(max_stack) = real_missing_value
    real :: sfc_wind_v_stack(max_stack) = real_missing_value
    integer :: sfc_wind_stack_qc(max_stack) = int_missing_value
    integer :: sfc_wind_stack_pc(max_stack) = int_missing_value
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

end module metar_mod
