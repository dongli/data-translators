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
    real   , allocatable :: p (:)
    real   , allocatable :: h (:)
    real   , allocatable :: wd(:)
    real   , allocatable :: ws(:)
    real   , allocatable :: ua(:)
    real   , allocatable :: va(:)
    integer, allocatable ::  p_qc(:)
    integer, allocatable ::  h_qc(:)
    integer, allocatable :: ua_qc(:)
    integer, allocatable :: va_qc(:)
    integer, allocatable :: wd_qc(:)
    integer, allocatable :: ws_qc(:)
    real   , allocatable ::  p_cr(:)
    real   , allocatable ::  h_cr(:)
    real   , allocatable :: ua_cr(:)
    real   , allocatable :: va_cr(:)
  contains
    procedure :: init => profiler_profile_init
    procedure :: set_from_hash => profiler_profile_set_from_hash
    final :: profiler_profile_final
  end type profiler_profile_type

  type profiler_profile_hash_type
    type(hash_table_type) p
    type(hash_table_type) p_qc
    type(hash_table_type) p_cr
    type(hash_table_type) h
    type(hash_table_type) h_qc
    type(hash_table_type) h_cr
    type(hash_table_type) ua
    type(hash_table_type) ua_qc
    type(hash_table_type) ua_cr
    type(hash_table_type) va
    type(hash_table_type) va_qc
    type(hash_table_type) va_cr
    type(hash_table_type) ws
    type(hash_table_type) ws_qc
    type(hash_table_type) wd
    type(hash_table_type) wd_qc
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
    allocate(this%p    (num_level))
    allocate(this%p_qc (num_level))
    allocate(this%p_cr (num_level))
    allocate(this%h    (num_level))
    allocate(this%h_qc (num_level))
    allocate(this%h_cr (num_level))
    allocate(this%ua   (num_level))
    allocate(this%ua_qc(num_level))
    allocate(this%ua_cr(num_level))
    allocate(this%va   (num_level))
    allocate(this%va_qc(num_level))
    allocate(this%va_cr(num_level))
    allocate(this%wd   (num_level))
    allocate(this%wd_qc(num_level))
    allocate(this%ws   (num_level))
    allocate(this%ws_qc(num_level))

  end subroutine profiler_profile_init

  subroutine profiler_profile_set_from_hash(this, hash)

    class(profiler_profile_type), intent(inout) :: this
    type(profiler_profile_hash_type), intent(inout), pointer :: hash

    integer i
    type(hash_table_iterator_type) level_iterator

    i = 1
    if (hash%p%size /= 0) then
      level_iterator = hash_table_iterator(hash%p)
    else
      level_iterator = hash_table_iterator(hash%h)
    end if
    do while (.not. level_iterator%ended())
      if (hash%p%size /= 0) then
        ! pressure (Pa)
        select type (value => level_iterator%value)
        type is (real)
          this%p(i) = value
        class default
          this%p(i) = real_missing_value
        end select
        select type (value => hash%p_qc%value(level_iterator%key))
        type is (integer)
          this%p_qc(i) = value
        class default
          this%p_qc(i) = int_missing_value
        end select
        select type (value => hash%p_cr%value(level_iterator%key))
        type is (real)
          this%p_cr(i) = value
        class default
          this%p_cr(i) = real_missing_value
        end select
      else
        this%p(i)    = real_missing_value
        this%p_qc(i) = int_missing_value
        this%p_cr(i) = real_missing_value
      end if
      ! height (m)
      select type (value => hash%h%value(level_iterator%key))
      type is (real)
        this%h(i) = value
      class default
        this%h(i) = real_missing_value
      end select
      select type (value => hash%h_qc%value(level_iterator%key))
      type is (integer)
        this%h_qc(i) = value
      class default
        this%h_qc(i) = int_missing_value
      end select
      select type (value => hash%h_cr%value(level_iterator%key))
      type is (real)
        this%h_cr(i) = value
      class default
        this%h_cr(i) = real_missing_value
      end select
      ! wind u component (m/s)
      select type (value => hash%ua%value(level_iterator%key))
      type is (real)
        this%ua(i) = value
      class default
        this%ua(i) = real_missing_value
      end select
      select type (value => hash%ua_qc%value(level_iterator%key))
      type is (integer)
        this%ua_qc(i) = value
      class default
        this%ua_qc(i) = int_missing_value
      end select
      select type (value => hash%ua_cr%value(level_iterator%key))
      type is (real)
        this%ua_cr(i) = value
      class default
        this%ua_cr(i) = real_missing_value
      end select
      ! wind v component (m/s)
      select type (value => hash%va%value(level_iterator%key))
      type is (real)
        this%va(i) = value
      class default
        this%va(i) = real_missing_value
      end select
      select type (value => hash%va_qc%value(level_iterator%key))
      type is (integer)
        this%va_qc(i) = value
      class default
        this%va_qc(i) = int_missing_value
      end select
      select type (value => hash%va_cr%value(level_iterator%key))
      type is (real)
        this%va_cr(i) = value
      class default
        this%va_cr(i) = real_missing_value
      end select
      ! wind speed (m/s)
      select type (value => hash%ws%value(level_iterator%key))
      type is (real)
        this%ws(i) = value
      class default
        this%ws(i) = real_missing_value
      end select
      select type (value => hash%ws_qc%value(level_iterator%key))
      type is (integer)
        this%ws_qc(i) = value
      class default
        this%ws_qc(i) = int_missing_value
      end select
      ! wind direction (degree)
      select type (value => hash%wd%value(level_iterator%key))
      type is (real)
        this%wd(i) = value
      class default
        this%wd(i) = real_missing_value
      end select
      select type (value => hash%wd_qc%value(level_iterator%key))
      type is (integer)
        this%wd_qc(i) = value
      class default
        this%wd_qc(i) = int_missing_value
      end select
      i = i + 1
      call level_iterator%next()
    end do

    ! Release memory of hash.
    deallocate(hash)

  end subroutine profiler_profile_set_from_hash

  subroutine profiler_profile_final(this)

    type(profiler_profile_type), intent(inout) :: this

    if (allocated(this%p    )) deallocate(this%p)
    if (allocated(this%p_qc )) deallocate(this%p_qc)
    if (allocated(this%p_cr )) deallocate(this%p_cr)
    if (allocated(this%h    )) deallocate(this%h)
    if (allocated(this%h_qc )) deallocate(this%h_qc)
    if (allocated(this%h_cr )) deallocate(this%h_cr)
    if (allocated(this%ua   )) deallocate(this%ua)
    if (allocated(this%ua_qc)) deallocate(this%ua_qc)
    if (allocated(this%ua_cr)) deallocate(this%ua_cr)
    if (allocated(this%va   )) deallocate(this%va)
    if (allocated(this%va_qc)) deallocate(this%va_qc)
    if (allocated(this%va_cr)) deallocate(this%va_cr)
    if (allocated(this%ws   )) deallocate(this%ws)
    if (allocated(this%ws_qc)) deallocate(this%ws_qc)
    if (allocated(this%wd   )) deallocate(this%wd)
    if (allocated(this%wd_qc)) deallocate(this%wd_qc)

  end subroutine profiler_profile_final

  subroutine profiler_profile_hash_init(this)

    class(profiler_profile_hash_type), intent(inout) :: this

    this%p     = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%p_qc  = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%p_cr  = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%h     = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%h_qc  = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%h_cr  = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%ua    = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%ua_qc = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%ua_cr = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%va    = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%va_qc = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%va_cr = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%wd    = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%wd_qc = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%ws    = hash_table(chunk_size=1000, max_load_factor=0.9)
    this%ws_qc = hash_table(chunk_size=1000, max_load_factor=0.9)

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
      if (is_missing(this%pro%p(i))) then
        write(*, '(A15, A5)', advance='no') 'X', '(X)'
      else
        write(*, '(F15.1, "(", I3, ")")', advance='no') this%pro%p(i), this%pro%p_qc(i)
      end if
      if (is_missing(this%pro%h(i))) then 
        write(*, '(A15, A5)', advance='no') 'X', '(X)'
      else
        write(*, '(F15.1, "(", I3, ")")', advance='no') this%pro%h(i), this%pro%h_qc(i)
      end if
      if (is_missing(this%pro%ua(i))) then 
        write(*, '(A15, A5)', advance='no') 'X', '(X)'
      else
        write(*, '(F15.1, "(", I3, ")")', advance='no') this%pro%ua(i), this%pro%ua_qc(i)
      end if
      if (is_missing(this%pro%va(i))) then 
        write(*, '(A15, A5)', advance='no') 'X', '(X)'
      else
        write(*, '(F15.1, "(", I3, ")")', advance='no') this%pro%va(i), this%pro%va_qc(i)
      end if
      if (is_missing(this%pro%wd(i))) then 
        write(*, '(A15, A5)', advance='no') 'X', '(X)'
      else
        write(*, '(F15.1, "(", I3, ")")', advance='no') this%pro%wd(i), this%pro%wd_qc(i)
      end if
      if (is_missing(this%pro%ws(i))) then 
        write(*, '(A15, A5)', advance='no') 'X', '(X)'
      else
        write(*, '(F15.1, "(", I3, ")")', advance='no') this%pro%ws(i), this%pro%ws_qc(i)
      end if
      write(*, *)
    end do

  end subroutine profiler_record_print

end module profiler_mod
