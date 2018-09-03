module synop_mod

  use obs_base_mod
  use params_mod
  use hash_table_mod
  use linked_list_mod

  implicit none

  type, extends(obs_station_type) :: synop_station_type
  end type synop_station_type

  integer, parameter :: max_stack = 255

  type, extends(obs_static_record_base_type) :: synop_record_type
    type(synop_station_type), pointer :: station
    real :: sfc_temperature      = real_missing_value ! Temperature (degC)
    real :: sfc_dewpoint         = real_missing_value ! Dewpoint temperature (degC)
    real :: sfc_pressure         = real_missing_value ! Surface pressure (Pa)
    real :: sfc_relative_humdity = real_missing_value ! Relative humidity (%)
    real :: sfc_wind_speed       = real_missing_value ! Wind speed (m/s)
    real :: sfc_wind_direction   = real_missing_value ! Wind direction (deg)
    real :: sfc_rain_01h         = real_missing_value ! 1h accumulated total precipitation (mm)
    real :: sfc_rain_03h         = real_missing_value ! 3h accumulated total precipitation (mm)
    real :: sfc_rain_06h         = real_missing_value ! 6h accumulated total precipitation (mm)
    real :: sfc_rain_12h         = real_missing_value ! 12h accumulated total precipitation (mm)
    real :: sfc_rain_24h         = real_missing_value ! 24h accumulated total precipitation (mm)
    real :: sfc_cloud_amount     = real_missing_value ! Cloud amount (???)

    real :: sfc_temperature_stack(max_stack) = real_missing_value
    integer :: sfc_temperature_qc(max_stack) = int_missing_value
    integer :: sfc_temperature_pc(max_stack) = int_missing_value

    real :: sfc_pressure_stack(max_stack) = real_missing_value
    integer :: sfc_pressure_qc(max_stack) = int_missing_value
    integer :: sfc_pressure_pc(max_stack) = int_missing_value

    real :: sfc_wind_u_stack(max_stack) = real_missing_value
    real :: sfc_wind_v_stack(max_stack) = real_missing_value
    integer :: sfc_wind_qc(max_stack) = int_missing_value
    integer :: sfc_wind_pc(max_stack) = int_missing_value
  end type synop_record_type

  type(hash_table_type) stations
  type(linked_list_type) records

end module synop_mod
