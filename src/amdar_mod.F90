module amdar_mod

  use obs_base_mod
  use params_mod
  use hash_table_mod
  use linked_list_mod

  implicit none

  type, extends(obs_site_nopos_base_type) :: amdar_flight_type
  end type amdar_flight_type

  type, extends(obs_drift_record_base_type) :: amdar_record_type
    type(amdar_flight_type), pointer :: flight
    real :: amdar_pressure          = real_missing_value ! Pressure (Pa)
    real :: amdar_temperature       = real_missing_value ! Temperature (degC)
    real :: amdar_specific_humidity = real_missing_value ! Specific humidity (g/kg)
    real :: amdar_dewpoint          = real_missing_value ! Dewpoint temperature (degC)
    real :: amdar_relative_humidity = real_missing_value ! Relative humidity (%)
    real :: amdar_wind_speed        = real_missing_value ! Wind speed (m/s)
    real :: amdar_wind_direction    = real_missing_value ! Wind direction (deg)

    integer :: amdar_pressure_qc          = int_missing_value
    integer :: amdar_temperature_qc       = int_missing_value
    integer :: amdar_dewpoint_qc          = int_missing_value
    integer :: amdar_specific_humidity_qc = int_missing_value
    integer :: amdar_relative_humidity_qc = int_missing_value
    integer :: amdar_wind_qc              = int_missing_value
  end type amdar_record_type

  type(hash_table_type)  flights
  type(linked_list_type) records

end module amdar_mod
