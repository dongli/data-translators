module raob_mod

  use obs_base_mod
  use params_mod
  use hash_table_mod
  use linked_list_mod

  implicit none

  type, extends(obs_station_type) :: raob_station_type
  end type raob_station_type

  type, extends(obs_static_record_base_type) :: raob_record_type
    type(raob_station_type), pointer :: station
    integer :: num_level = 0
    real, allocatable :: height_levels(:)
  end type raob_record_type

  type(hash_table_type) stations
  type(linked_list_type) records

end module raob_mod