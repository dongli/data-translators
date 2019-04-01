! NOTE: seq_id is zero-started.

module obs_base_mod

  use datetime_mod
  use missing_value_mod

  implicit none

  type, abstract :: obs_site_base_type
    real :: lon = real_missing_value
    real :: lat = real_missing_value
    real :: z   = real_missing_value ! Height (m)
    real :: p   = real_missing_value ! Pressure (Pa)
  end type obs_site_base_type

  type, extends(obs_site_base_type) :: obs_station_type
    integer :: seq_id = 0
    character(30) name
  end type obs_station_type

  type, abstract :: obs_site_nopos_base_type
    integer :: seq_id = 0
    character(30) name
  end type obs_site_nopos_base_type

  type, abstract :: obs_static_record_base_type
    integer :: seq_id = 0
    type(datetime_type) time
    character(10) :: source = str_missing_value
  end type obs_static_record_base_type

  type, extends(obs_site_base_type), abstract :: obs_drift_record_base_type
    integer :: seq_id = 0
    type(datetime_type) time
    character(10) :: source = str_missing_value
  end type obs_drift_record_base_type

end module obs_base_mod
