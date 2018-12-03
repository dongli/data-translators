module raob_mod

  use obs_base_mod
  use params_mod
  use hash_table_mod
  use linked_list_mod

  implicit none

  type, extends(obs_station_type) :: raob_station_type
    type(linked_list_type), pointer :: records => null()
  contains
    procedure :: init => raob_station_init
    final :: raob_station_final
  end type raob_station_type

  type raob_profile_type
    integer :: num_level = 0
    real, allocatable :: pressure(:)
    real, allocatable :: height(:)
    real, allocatable :: temperature(:)
    real, allocatable :: dewpoint(:)
    real, allocatable :: specific_humidity(:)
    real, allocatable :: relative_humidity(:)
    real, allocatable :: wind_direction(:)
    real, allocatable :: wind_speed(:)
    real, allocatable :: wind_u(:)
    real, allocatable :: wind_v(:)
  contains
    procedure :: init => raob_profile_init
    procedure :: set_from_hash => raob_profile_set_from_hash
    final :: raob_profile_final
  end type raob_profile_type

  type raob_profile_hash_type
    type(hash_table_type) pressure
    type(hash_table_type) height
    type(hash_table_type) temperature
    type(hash_table_type) dewpoint
    type(hash_table_type) specific_humidity
    type(hash_table_type) relative_humidity
    type(hash_table_type) wind_direction
    type(hash_table_type) wind_speed
    type(hash_table_type) wind_u
    type(hash_table_type) wind_v
  contains
    procedure :: init => raob_profile_hash_init
  end type raob_profile_hash_type

  type, extends(obs_static_record_base_type) :: raob_record_type
    type(raob_station_type), pointer :: station
    real :: snd_sfc_pressure          = real_missing_value
    real :: snd_sfc_temperature       = real_missing_value
    real :: snd_sfc_dewpoint          = real_missing_value
    real :: snd_sfc_specific_humidity = real_missing_value
    real :: snd_sfc_wind_direction    = real_missing_value
    real :: snd_sfc_wind_speed        = real_missing_value
    real :: snd_sfc_wind_u            = real_missing_value
    real :: snd_sfc_wind_v            = real_missing_value
    type(raob_profile_type) snd_man
    type(raob_profile_type) snd_sigt
    type(raob_profile_type) snd_sigw
    type(raob_profile_type) snd_trop
    type(raob_profile_hash_type), pointer :: snd_man_hash => null()
    type(raob_profile_hash_type), pointer :: snd_sigt_hash => null()
    type(raob_profile_hash_type), pointer :: snd_sigw_hash => null()
    type(raob_profile_hash_type), pointer :: snd_trop_hash => null()
  contains
    procedure :: init => raob_record_init
  end type raob_record_type

contains

  subroutine raob_station_init(this, name, lon, lat, z)

    class(raob_station_type), intent(inout) :: this
    character(*), intent(in) :: name
    real, intent(in) :: lon
    real, intent(in) :: lat
    real, intent(in) :: z

    this%name = name
    this%lon = lon
    this%lat = lat
    this%z = z
    if (.not. associated(this%records)) allocate(this%records)

  end subroutine raob_station_init

  subroutine raob_station_final(this)

    type(raob_station_type), intent(inout) :: this

    if (associated(this%records)) deallocate(this%records)

  end subroutine raob_station_final

  subroutine raob_profile_init(this, num_level)

    class(raob_profile_type), intent(inout) :: this
    integer, intent(in) :: num_level

    this%num_level = num_level
    call raob_profile_final(this)
    allocate(this%pressure(num_level))
    allocate(this%height(num_level))
    allocate(this%temperature(num_level))
    allocate(this%dewpoint(num_level))
    allocate(this%specific_humidity(num_level))
    allocate(this%relative_humidity(num_level))
    allocate(this%wind_direction(num_level))
    allocate(this%wind_speed(num_level))
    allocate(this%wind_u(num_level))
    allocate(this%wind_v(num_level))

  end subroutine raob_profile_init

  subroutine raob_profile_set_from_hash(this, hash)

    class(raob_profile_type), intent(inout) :: this
    type(raob_profile_hash_type), intent(inout), pointer :: hash

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
      ! temperature (degC)
      select type (value => hash%temperature%value(level_iterator%key))
      type is (real)
        this%temperature(i) = value
      class default
        this%temperature(i) = real_missing_value
      end select
      ! dewpoint (degC)
      select type (value => hash%dewpoint%value(level_iterator%key))
      type is (real)
        this%dewpoint(i) = value
      class default
        this%dewpoint(i) = real_missing_value
      end select
      ! specific humidity (Mg/Kg)
      select type (value => hash%specific_humidity%value(level_iterator%key))
      type is (real)
        this%specific_humidity(i) = value
      class default
        this%specific_humidity(i) = real_missing_value
      end select
      ! relative humidity (%)
      select type (value => hash%relative_humidity%value(level_iterator%key))
      type is (real)
        this%relative_humidity(i) = value
      class default
        this%relative_humidity(i) = real_missing_value
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

  end subroutine raob_profile_set_from_hash

  subroutine raob_profile_final(this)

    type(raob_profile_type), intent(inout) :: this

    if (allocated(this%pressure))           deallocate(this%pressure)
    if (allocated(this%height))             deallocate(this%height)
    if (allocated(this%temperature))        deallocate(this%temperature)
    if (allocated(this%dewpoint))           deallocate(this%dewpoint)
    if (allocated(this%specific_humidity))  deallocate(this%specific_humidity)
    if (allocated(this%relative_humidity))  deallocate(this%relative_humidity)
    if (allocated(this%wind_direction))     deallocate(this%wind_direction)
    if (allocated(this%wind_speed))         deallocate(this%wind_speed)
    if (allocated(this%wind_u))             deallocate(this%wind_u)
    if (allocated(this%wind_v))             deallocate(this%wind_v)

  end subroutine raob_profile_final

  subroutine raob_profile_hash_init(this)

    class(raob_profile_hash_type), intent(inout) :: this

    this%pressure           = hash_table(chunk_size=100, max_load_factor=0.9)
    this%height             = hash_table(chunk_size=100, max_load_factor=0.9)
    this%temperature        = hash_table(chunk_size=100, max_load_factor=0.9)
    this%dewpoint           = hash_table(chunk_size=100, max_load_factor=0.9)
    this%specific_humidity  = hash_table(chunk_size=100, max_load_factor=0.9)
    this%relative_humidity  = hash_table(chunk_size=100, max_load_factor=0.9)
    this%wind_direction     = hash_table(chunk_size=100, max_load_factor=0.9)
    this%wind_speed         = hash_table(chunk_size=100, max_load_factor=0.9)
    this%wind_u             = hash_table(chunk_size=100, max_load_factor=0.9)
    this%wind_v             = hash_table(chunk_size=100, max_load_factor=0.9)

  end subroutine raob_profile_hash_init

  subroutine raob_record_init(this, alloc_hash)

    class(raob_record_type), intent(out) :: this
    logical, intent(in), optional :: alloc_hash

    if (present(alloc_hash) .and. alloc_hash) then
      allocate(this%snd_man_hash);  call this%snd_man_hash %init()
      allocate(this%snd_sigt_hash); call this%snd_sigt_hash%init()
      allocate(this%snd_sigw_hash); call this%snd_sigw_hash%init()
      allocate(this%snd_trop_hash); call this%snd_trop_hash%init()
    end if

  end subroutine raob_record_init

end module raob_mod
