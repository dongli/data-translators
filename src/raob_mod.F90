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

  type, extends(obs_static_record_base_type) :: raob_decode_record_type
    type(raob_station_type), pointer :: station
    type(hash_table_type) snd_man_temperature
    type(hash_table_type) snd_man_specific_humidity
    type(hash_table_type) snd_man_dewpoint
    type(hash_table_type) snd_man_pressure
    type(hash_table_type) snd_man_wind_speed
    type(hash_table_type) snd_man_wind_direction
    type(hash_table_type) snd_sig_temperature
    type(hash_table_type) snd_sig_specific_humidity
    type(hash_table_type) snd_sig_dewpoint
    type(hash_table_type) snd_sig_pressure
    type(hash_table_type) snd_sig_wind_speed
    type(hash_table_type) snd_sig_wind_direction
    type(hash_table_type) snd_wnd_pressure
    type(hash_table_type) snd_wnd_wind_speed
    type(hash_table_type) snd_wnd_wind_direction
    type(hash_table_type) snd_trop_temperature
    type(hash_table_type) snd_trop_specific_humidity
    type(hash_table_type) snd_trop_dewpoint
    type(hash_table_type) snd_trop_pressure
    type(hash_table_type) snd_trop_wind_speed
    type(hash_table_type) snd_trop_wind_direction
  contains
    procedure :: init => raob_decode_record_init
  end type raob_decode_record_type

  type(hash_table_type) stations
  type(linked_list_type) records

contains

  subroutine raob_decode_record_init(this)

    class(raob_decode_record_type), intent(out) :: this

    this%snd_man_temperature        = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%snd_man_specific_humidity  = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%snd_man_dewpoint           = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%snd_man_pressure           = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%snd_man_wind_direction     = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%snd_man_wind_speed         = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%snd_sig_temperature        = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%snd_sig_specific_humidity  = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%snd_sig_dewpoint           = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%snd_sig_pressure           = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%snd_sig_wind_direction     = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%snd_sig_wind_speed         = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%snd_wnd_pressure           = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%snd_wnd_wind_direction     = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%snd_wnd_wind_speed         = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%snd_trop_temperature       = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%snd_trop_specific_humidity = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%snd_trop_dewpoint          = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%snd_trop_pressure          = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%snd_trop_wind_direction    = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%snd_trop_wind_speed        = hash_table(chunk_size=1000, max_load_factor=0.9)

  end subroutine raob_decode_record_init

end module raob_mod