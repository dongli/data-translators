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
    integer :: snd_num_man_level = 0
    integer :: snd_num_sig_level = 0
    real, allocatable :: snd_man_height(:)
    real, allocatable :: snd_man_temperature(:)
    real, allocatable :: snd_man_dewpoint(:)
    real, allocatable :: snd_man_pressure(:)
    real, allocatable :: snd_man_wind_speed(:)
    real, allocatable :: snd_man_wind_direction(:)
    real, allocatable :: snd_sig_temperature(:)
    real, allocatable :: snd_sig_dewpoint(:)
    real, allocatable :: snd_sig_pressure(:)
    real, allocatable :: snd_sig_wind_speed(:)
    real, allocatable :: snd_sig_wind_direction(:)
  end type raob_record_type

  type(hash_table_type) stations
  type(linked_list_type) records

end module raob_mod