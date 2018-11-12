module ship_mod

  use obs_base_mod
  use params_mod
  use hash_table_mod
  use linked_list_mod

  implicit none

  type, extends(obs_site_nopos_base_type) :: ship_type
  end type ship_type

  type, extends(obs_drift_record_base_type) :: ship_record_type
    type(ship_type), pointer :: ship
    real    :: ship_pressure          = real_missing_value ! Pressure (Pa)
    real    :: ship_air_temperature   = real_missing_value ! Air temperature (degC)
    real    :: ship_sea_temperature   = real_missing_value ! Sea temperature / SST (degC)
    real    :: ship_dewpoint          = real_missing_value ! Dewpoint temperature (degC)
    real    :: ship_relative_humidity = real_missing_value ! Relative humidity (%)
    real    :: ship_specific_humidity = real_missing_value ! Specific humidity (Mg/Kg)
    real    :: ship_wind_speed        = real_missing_value ! Wind speed (m/s)
    real    :: ship_wind_direction    = real_missing_value ! Wind direction (deg)
    real    :: ship_wind_u            = real_missing_value ! U wind component (m/s)
    real    :: ship_wind_v            = real_missing_value ! V wind component (m/s)

    integer :: ship_pressure_qc          = int_missing_value
    integer :: ship_air_temperature_qc   = int_missing_value
    integer :: ship_sea_temperature_qc   = int_missing_value
    integer :: ship_dewpoint_qc          = int_missing_value
    integer :: ship_relative_humidity_qc = int_missing_value
    integer :: ship_specific_humidity_qc = int_missing_value
    integer :: ship_wind_qc              = int_missing_value
  end type ship_record_type

end module ship_mod