module synop_mod

  use obs_base_mod
  use params_mod

  implicit none

  type, extends(obs_station_type) :: synop_station_type
  end type synop_station_type

  type, extends(obs_static_record_base_type) :: synop_record_type
    type(synop_station_type), pointer :: station
    real :: T    = real_missing_value ! Temperature (degC)
    real :: TD   = real_missing_value ! Dewpoint temperature (degC)
    real :: PS   = real_missing_value ! Surface pressure (Pa)
    real :: RH   = real_missing_value ! Relative humidity (%)
    real :: WS   = real_missing_value ! Wind speed (m/s)
    real :: WD   = real_missing_value ! Wind direction (deg)
    real :: TP01 = real_missing_value ! 1h accumulated total precipitation (mm)
    real :: TP03 = real_missing_value ! 3h accumulated total precipitation (mm)
    real :: TP06 = real_missing_value ! 6h accumulated total precipitation (mm)
    real :: TP12 = real_missing_value ! 12h accumulated total precipitation (mm)
    real :: TP24 = real_missing_value ! 24h accumulated total precipitation (mm)
    real :: CLD  = real_missing_value ! Cloud amount (???)
  end type synop_record_type

end module synop_mod
