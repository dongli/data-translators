module amdar_mod

  use obs_base_mod
  use params_mod

  implicit none

  type, extends(obs_site_nopos_base_type) :: amdar_flight_type
  end type amdar_flight_type

  type, extends(obs_drift_record_base_type) :: amdar_record_type
    type(amdar_flight_type), pointer :: flight
    real :: T  = real_missing_value ! Temperature (degC)
    real :: SH = real_missing_value ! Specific humidity (g/kg)
    real :: TD = real_missing_value ! Dewpoint temperature (degC)
    real :: RH = real_missing_value ! Relative humidity (%)
    real :: WS = real_missing_value ! Wind speed (m/s)
    real :: WD = real_missing_value ! Wind direction (deg)
  end type amdar_record_type

end module amdar_mod
