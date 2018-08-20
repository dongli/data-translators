module raob_mod

  use obs_base_mod

  implicit none

  type, extends(obs_station_type) :: raob_station_type
  end type raob_station_type

  type, extends(obs_static_record_base_type) :: raob_record_type
    integer :: num_level = 0
    real, allocatable :: height_levels(:)
  end type raob_record_type

end module raob_mod