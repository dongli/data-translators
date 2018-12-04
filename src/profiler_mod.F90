module profiler_mod

  use obs_base_mod
  use params_mod
  use hash_table_mod
  use linked_list_mod

  implicit none

  type, extends(obs_station_type) :: profiler_station_type
    type(linked_list_type), pointer :: records => null()
  contains
    procedure :: init => profiler_station_init
    final :: profiler_station_final
  end type profiler_station_type

  type profiler_profile_type
    integer :: num_level = 0
    real, allocatable :: pressure(:)
    real, allocatable :: height(:)
    real, allocatable :: wind_direction(:)
    real, allocatable :: wind_speed(:)
    real, allocatable :: wind_u(:)
    real, allocatable :: wind_v(:)
  contains
    procedure :: init => profiler_profile_init
    procedure :: set_from_hash => profiler_profile_set_from_hash
    final :: profiler_profile_final
  end type profiler_profile_type

  type profiler_profile_hash_type
    type(hash_table_type) pressure
    type(hash_table_type) height
    type(hash_table_type) wind_u
    type(hash_table_type) wind_v
    type(hash_table_type) wind_speed
    type(hash_table_type) wind_direction
  contains
    procedure :: init => profiler_profile_hash_init
  end type profiler_profile_hash_type

  type, extends(obs_static_record_base_type) :: profiler_record_type
    type(profiler_station_type), pointer :: station
    type(profiler_profile_type) pro
    type(profiler_profile_hash_type), pointer :: pro_hash => null()
  contains
    procedure :: init => profiler_record_init
  end type profiler_record_type

contains

  subroutine profiler_station_init(this, name, lon, lat, z)

    class(profiler_station_type), intent(inout) :: this
    character(*), intent(in) :: name
    real, intent(in) :: lon
    real, intent(in) :: lat
    real, intent(in) :: z

    this%name = name
    this%lon = lon
    this%lat = lat
    this%z = z
    if (.not. associated(this%records)) allocate(this%records)

  end subroutine profiler_station_init

  subroutine profiler_station_final(this)

    type(profiler_station_type), intent(inout) :: this

    if (associated(this%records)) deallocate(this%records)

  end subroutine profiler_station_final

  subroutine profiler_profile_init(this, num_level)

    class(profiler_profile_type), intent(inout) :: this
    integer, intent(in) :: num_level

    this%num_level = num_level
    call profiler_profile_final(this)
    allocate(this%pressure(num_level))
    allocate(this%height(num_level))
    allocate(this%wind_direction(num_level))
    allocate(this%wind_speed(num_level))
    allocate(this%wind_u(num_level))
    allocate(this%wind_v(num_level))

  end subroutine profiler_profile_init

  subroutine profiler_profile_set_from_hash(this, hash)

    class(profiler_profile_type), intent(inout) :: this
    type(profiler_profile_hash_type), intent(inout), pointer :: hash

    integer i
    type(hash_table_iterator_type) level_iterator

    i = 1
    level_iterator = hash_table_iterator(hash%pressure)
    do while (.not. level_iterator%ended())
      ! pressure (Pa)
      select type (value => level_iterator%value)
      type is (real)
        this%pressure(i) = value
      class default
        this%pressure(i) = real_missing_value
      end select
      ! height (m)
      select type (value => hash%height%value(level_iterator%key))
      type is (real)
        this%height(i) = value
      class default
        this%height(i) = real_missing_value
      end select
      ! wind direction (degree)
      select type (value => hash%wind_direction%value(level_iterator%key))
      type is (real)
        this%wind_direction(i) = value
      class default
        this%wind_direction(i) = real_missing_value
      end select
      ! wind speed (m/s)
      select type (value => hash%wind_speed%value(level_iterator%key))
      type is (real)
        this%wind_speed(i) = value
      class default
        this%wind_speed(i) = real_missing_value
      end select
      ! wind u component (m/s)
      select type (value => hash%wind_u%value(level_iterator%key))
      type is (real)
        this%wind_u(i) = value
      class default
        this%wind_u(i) = real_missing_value
      end select
      ! wind v component (m/s)
      select type (value => hash%wind_v%value(level_iterator%key))
      type is (real)
        this%wind_v(i) = value
      class default
        this%wind_v(i) = real_missing_value
      end select
      i = i + 1
      call level_iterator%next()
    end do

    ! Release memory of hash.
    deallocate(hash)

  end subroutine profiler_profile_set_from_hash

  subroutine profiler_profile_final(this)

    type(profiler_profile_type), intent(inout) :: this

    if (allocated(this%pressure))       deallocate(this%pressure)
    if (allocated(this%height))         deallocate(this%height)
    if (allocated(this%wind_direction)) deallocate(this%wind_direction)
    if (allocated(this%wind_speed))     deallocate(this%wind_speed)
    if (allocated(this%wind_u))         deallocate(this%wind_u)
    if (allocated(this%wind_v))         deallocate(this%wind_v)

  end subroutine profiler_profile_final

  subroutine profiler_profile_hash_init(this)

    class(profiler_profile_hash_type), intent(inout) :: this

    this%pressure       = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%height         = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%wind_u         = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%wind_v         = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%wind_direction = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%wind_speed     = hash_table(chunk_size=1000, max_load_factor=0.9)

  end subroutine profiler_profile_hash_init

  subroutine profiler_record_init(this, alloc_hash)

    class(profiler_record_type), intent(out) :: this
    logical, intent(in), optional :: alloc_hash

    if (present(alloc_hash) .and. alloc_hash) then
      allocate(this%pro_hash)
      call this%pro_hash%init()
    end if

  end subroutine profiler_record_init

end module profiler_mod
