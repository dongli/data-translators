module profiler_mod

  use obs_base_mod
  use params_mod
  use hash_table_mod
  use linked_list_mod
  use missing_value_mod

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
    integer, allocatable :: pressure_qc(:)
    integer, allocatable :: height_qc(:)
    integer, allocatable :: wind_qc(:)
    real, allocatable :: pressure_correct(:)
    real, allocatable :: height_correct(:)
    real, allocatable :: wind_u_correct(:)
    real, allocatable :: wind_v_correct(:)
  contains
    procedure :: init => profiler_profile_init
    procedure :: set_from_hash => profiler_profile_set_from_hash
    final :: profiler_profile_final
  end type profiler_profile_type

  type profiler_profile_hash_type
    type(hash_table_type) pressure
    type(hash_table_type) pressure_qc
    type(hash_table_type) pressure_correct
    type(hash_table_type) height
    type(hash_table_type) height_qc
    type(hash_table_type) height_correct
    type(hash_table_type) wind_u
    type(hash_table_type) wind_u_correct
    type(hash_table_type) wind_v
    type(hash_table_type) wind_v_correct
    type(hash_table_type) wind_speed
    type(hash_table_type) wind_direction
    type(hash_table_type) wind_qc
  contains
    procedure :: init => profiler_profile_hash_init
  end type profiler_profile_hash_type

  type, extends(obs_static_record_base_type) :: profiler_record_type
    type(profiler_station_type), pointer :: station
    type(profiler_profile_type) pro
    type(profiler_profile_hash_type), pointer :: pro_hash => null()
  contains
    procedure :: init => profiler_record_init
    procedure :: print => profiler_record_print
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
    allocate(this%pressure_qc(num_level))
    allocate(this%pressure_correct(num_level))
    allocate(this%height(num_level))
    allocate(this%height_qc(num_level))
    allocate(this%height_correct(num_level))
    allocate(this%wind_u(num_level))
    allocate(this%wind_u_correct(num_level))
    allocate(this%wind_v(num_level))
    allocate(this%wind_v_correct(num_level))
    allocate(this%wind_speed(num_level))
    allocate(this%wind_direction(num_level))
    allocate(this%wind_qc(num_level))

  end subroutine profiler_profile_init

  subroutine profiler_profile_set_from_hash(this, hash)

    class(profiler_profile_type), intent(inout) :: this
    type(profiler_profile_hash_type), intent(inout), pointer :: hash

    integer i
    type(hash_table_iterator_type) level_iterator

    i = 1
    if (hash%pressure%size /= 0) then
      level_iterator = hash_table_iterator(hash%pressure)
    else
      level_iterator = hash_table_iterator(hash%height)
    end if
    do while (.not. level_iterator%ended())
      if (hash%pressure%size /= 0) then
        ! pressure (Pa)
        select type (value => level_iterator%value)
        type is (real)
          this%pressure(i) = value
        class default
          this%pressure(i) = real_missing_value
        end select
        select type (value => hash%pressure_qc%value(level_iterator%key))
        type is (integer)
          this%pressure_qc(i) = value
        class default
          this%pressure_qc(i) = int_missing_value
        end select
        select type (value => hash%pressure_correct%value(level_iterator%key))
        type is (real)
          this%pressure_correct(i) = value
        class default
          this%pressure_correct(i) = real_missing_value
        end select
      else
        this%pressure(i) = real_missing_value
        this%pressure_qc(i) = int_missing_value
        this%pressure_correct(i) = real_missing_value
      end if
      ! height (m)
      select type (value => hash%height%value(level_iterator%key))
      type is (real)
        this%height(i) = value
      class default
        this%height(i) = real_missing_value
      end select
      select type (value => hash%height_qc%value(level_iterator%key))
      type is (integer)
        this%height_qc(i) = value
      class default
        this%height_qc(i) = int_missing_value
      end select
      select type (value => hash%height_correct%value(level_iterator%key))
      type is (real)
        this%height_correct(i) = value
      class default
        this%height_correct(i) = real_missing_value
      end select
      ! wind u component (m/s)
      select type (value => hash%wind_u%value(level_iterator%key))
      type is (real)
        this%wind_u(i) = value
      class default
        this%wind_u(i) = real_missing_value
      end select
      select type (value => hash%wind_u_correct%value(level_iterator%key))
      type is (real)
        this%wind_u_correct(i) = value
      class default
        this%wind_u_correct(i) = real_missing_value
      end select
      ! wind v component (m/s)
      select type (value => hash%wind_v%value(level_iterator%key))
      type is (real)
        this%wind_v(i) = value
      class default
        this%wind_v(i) = real_missing_value
      end select
      select type (value => hash%wind_v_correct%value(level_iterator%key))
      type is (real)
        this%wind_v_correct(i) = value
      class default
        this%wind_v_correct(i) = real_missing_value
      end select
      ! wind speed (m/s)
      select type (value => hash%wind_speed%value(level_iterator%key))
      type is (real)
        this%wind_speed(i) = value
      class default
        this%wind_speed(i) = real_missing_value
      end select
      ! wind direction (degree)
      select type (value => hash%wind_direction%value(level_iterator%key))
      type is (real)
        this%wind_direction(i) = value
      class default
        this%wind_direction(i) = real_missing_value
      end select
      select type (value => hash%wind_qc%value(level_iterator%key))
      type is (integer)
        this%wind_qc(i) = value
      class default
        this%wind_qc(i) = int_missing_value
      end select
      i = i + 1
      call level_iterator%next()
    end do

    ! Release memory of hash.
    deallocate(hash)

  end subroutine profiler_profile_set_from_hash

  subroutine profiler_profile_final(this)

    type(profiler_profile_type), intent(inout) :: this

    if (allocated(this%pressure))         deallocate(this%pressure)
    if (allocated(this%pressure_qc))      deallocate(this%pressure_qc)
    if (allocated(this%pressure_correct)) deallocate(this%pressure_correct)
    if (allocated(this%height))           deallocate(this%height)
    if (allocated(this%height_qc))        deallocate(this%height_qc)
    if (allocated(this%height_correct))   deallocate(this%height_correct)
    if (allocated(this%wind_u))           deallocate(this%wind_u)
    if (allocated(this%wind_u_correct))   deallocate(this%wind_u_correct)
    if (allocated(this%wind_v))           deallocate(this%wind_v)
    if (allocated(this%wind_v_correct))   deallocate(this%wind_v_correct)
    if (allocated(this%wind_speed))       deallocate(this%wind_speed)
    if (allocated(this%wind_direction))   deallocate(this%wind_direction)
    if (allocated(this%wind_qc))          deallocate(this%wind_qc)

  end subroutine profiler_profile_final

  subroutine profiler_profile_hash_init(this)

    class(profiler_profile_hash_type), intent(inout) :: this

    this%pressure         = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%pressure_qc      = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%pressure_correct = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%height           = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%height_qc        = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%height_correct   = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%wind_u           = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%wind_u_correct   = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%wind_v           = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%wind_v_correct   = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%wind_speed       = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%wind_direction   = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%wind_qc          = hash_table(chunk_size=1000, max_load_factor=0.9)

  end subroutine profiler_profile_hash_init

  subroutine profiler_record_init(this, alloc_hash)

    class(profiler_record_type), intent(out) :: this
    logical, intent(in), optional :: alloc_hash

    if (present(alloc_hash) .and. alloc_hash) then
      allocate(this%pro_hash)
      call this%pro_hash%init()
    end if

  end subroutine profiler_record_init

  subroutine profiler_record_print(this)

    class(profiler_record_type), intent(in) :: this

    integer i

    print *, 'Station ', this%station%name
    print *, 'Time ', this%time%isoformat()
    write(*, '(A15, A5)', advance='no') 'P', ''
    write(*, '(A15, A5)', advance='no') 'H', ''
    write(*, '(A15, A5)', advance='no') 'U', ''
    write(*, '(A15, A5)', advance='no') 'V', ''
    write(*, '(A15, A5)', advance='no') 'WD', ''
    write(*, '(A15, A5)', advance='no') 'WS', ''
    write(*, *)
    do i = 1, this%pro%num_level
      if (is_missing(this%pro%pressure(i))) then
        write(*, '(A15, A5)', advance='no') 'X', '(X)'
      else
        write(*, '(F15.1, "(", I3, ")")', advance='no') this%pro%pressure(i), this%pro%pressure_qc(i)
      end if
      if (is_missing(this%pro%height(i))) then 
        write(*, '(A15, A5)', advance='no') 'X', '(X)'
      else
        write(*, '(F15.1, "(", I3, ")")', advance='no') this%pro%height(i), this%pro%height_qc(i)
      end if
      if (is_missing(this%pro%wind_u(i))) then 
        write(*, '(A15, A5)', advance='no') 'X', '(X)'
      else
        write(*, '(F15.1, "(", I3, ")")', advance='no') this%pro%wind_u(i), this%pro%wind_qc(i)
      end if
      if (is_missing(this%pro%wind_v(i))) then 
        write(*, '(A15, A5)', advance='no') 'X', '(X)'
      else
        write(*, '(F15.1, "(", I3, ")")', advance='no') this%pro%wind_v(i), this%pro%wind_qc(i)
      end if
      if (is_missing(this%pro%wind_direction(i))) then 
        write(*, '(A15, A5)', advance='no') 'X', '(X)'
      else
        write(*, '(F15.1, "(", I3, ")")', advance='no') this%pro%wind_direction(i), this%pro%wind_qc(i)
      end if
      if (is_missing(this%pro%wind_speed(i))) then 
        write(*, '(A15, A5)', advance='no') 'X', '(X)'
      else
        write(*, '(F15.1, "(", I3, ")")', advance='no') this%pro%wind_speed(i), this%pro%wind_qc(i)
      end if
      write(*, *)
    end do

  end subroutine profiler_record_print

end module profiler_mod
