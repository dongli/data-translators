module profiler_mod

  use obs_base_mod
  use params_mod
  use hash_table_mod
  use linked_list_mod

  implicit none

  type, extends(obs_station_type) :: profiler_station_type
  end type profiler_station_type

  type, extends(obs_static_record_base_type) :: profiler_record_type
    type(profiler_station_type), pointer :: station
    integer :: pro_num_level = 0
    real, allocatable :: pro_pressure(:)
    real, allocatable :: pro_height(:)
    real, allocatable :: pro_wind_u(:)
    real, allocatable :: pro_wind_v(:)
    real, allocatable :: pro_wind_speed(:)
    real, allocatable :: pro_wind_direction(:)
  end type profiler_record_type

  type, extends(obs_static_record_base_type) :: profiler_read_record_type
    type(profiler_station_type), pointer :: station
    type(hash_table_type) pro_pressure
    type(hash_table_type) pro_height
    type(hash_table_type) pro_wind_u
    type(hash_table_type) pro_wind_v
    type(hash_table_type) pro_wind_speed
    type(hash_table_type) pro_wind_direction
  contains
    procedure :: init => profiler_read_record_init
  end type profiler_read_record_type

  type(hash_table_type) stations
  type(linked_list_type) records

contains

  subroutine profiler_read_record_init(this)

    class(profiler_read_record_type), intent(out) :: this

    this%pro_pressure       = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%pro_height         = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%pro_wind_u         = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%pro_wind_v         = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%pro_wind_direction = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%pro_wind_speed     = hash_table(chunk_size=1000, max_load_factor=0.9)

  end subroutine profiler_read_record_init

end module profiler_mod
