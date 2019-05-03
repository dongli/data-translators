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
    integer, allocatable :: pressure_qc(:)
    integer, allocatable :: height_qc(:)
    integer, allocatable :: temperature_qc(:)
    integer, allocatable :: specific_humidity_qc(:)
    integer, allocatable :: wind_qc(:)
    real, allocatable :: pressure_correct(:)
    real, allocatable :: height_correct(:)
    real, allocatable :: temperature_correct(:)
    real, allocatable :: specific_humidity_correct(:)
    real, allocatable :: wind_u_correct(:)
    real, allocatable :: wind_v_correct(:)
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
    type(hash_table_type) pressure_qc
    type(hash_table_type) height_qc
    type(hash_table_type) temperature_qc
    type(hash_table_type) specific_humidity_qc
    type(hash_table_type) wind_qc
    type(hash_table_type) pressure_correct
    type(hash_table_type) height_correct
    type(hash_table_type) temperature_correct
    type(hash_table_type) specific_humidity_correct
    type(hash_table_type) wind_u_correct
    type(hash_table_type) wind_v_correct
  contains
    procedure :: init => raob_profile_hash_init
  end type raob_profile_hash_type

  type, extends(obs_static_record_base_type) :: raob_record_type
    type(raob_station_type), pointer :: station
    real :: sfc_pressure          = real_missing_value
    real :: sfc_temperature       = real_missing_value
    real :: sfc_dewpoint          = real_missing_value
    real :: sfc_specific_humidity = real_missing_value
    real :: sfc_wind_direction    = real_missing_value
    real :: sfc_wind_speed        = real_missing_value
    real :: sfc_wind_u            = real_missing_value
    real :: sfc_wind_v            = real_missing_value
    integer :: sfc_pressure_qc            = int_missing_value
    integer :: sfc_temperature_qc         = int_missing_value
    integer :: sfc_specific_humidity_qc   = int_missing_value
    integer :: sfc_wind_qc                = int_missing_value
    real :: sfc_pressure_correct          = real_missing_value
    real :: sfc_temperature_correct       = real_missing_value
    real :: sfc_specific_humidity_correct = real_missing_value
    real :: sfc_wind_u_correct            = real_missing_value
    real :: sfc_wind_v_correct            = real_missing_value
    type(raob_profile_type) man
    type(raob_profile_type) sigt
    type(raob_profile_type) sigw
    type(raob_profile_type) trop
    type(raob_profile_hash_type), pointer :: man_hash => null()
    type(raob_profile_hash_type), pointer :: sigt_hash => null()
    type(raob_profile_hash_type), pointer :: sigw_hash => null()
    type(raob_profile_hash_type), pointer :: trop_hash => null()
  contains
    procedure :: init => raob_record_init
    procedure :: print => raob_record_print
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
    allocate(this%pressure_qc(num_level))
    allocate(this%pressure_correct(num_level))
    allocate(this%height(num_level))
    allocate(this%height_qc(num_level))
    allocate(this%height_correct(num_level))
    allocate(this%temperature(num_level))
    allocate(this%temperature_qc(num_level))
    allocate(this%temperature_correct(num_level))
    allocate(this%dewpoint(num_level))
    allocate(this%specific_humidity(num_level))
    allocate(this%specific_humidity_qc(num_level))
    allocate(this%specific_humidity_correct(num_level))
    allocate(this%relative_humidity(num_level))
    allocate(this%wind_direction(num_level))
    allocate(this%wind_speed(num_level))
    allocate(this%wind_u(num_level))
    allocate(this%wind_u_correct(num_level))
    allocate(this%wind_v(num_level))
    allocate(this%wind_v_correct(num_level))
    allocate(this%wind_qc(num_level))

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
      ! temperature (degC)
      select type (value => hash%temperature%value(level_iterator%key))
      type is (real)
        this%temperature(i) = value
      class default
        this%temperature(i) = real_missing_value
      end select
      select type (value => hash%temperature_qc%value(level_iterator%key))
      type is (integer)
        this%temperature_qc(i) = value
      class default
        this%temperature_qc(i) = int_missing_value
      end select
      select type (value => hash%temperature_correct%value(level_iterator%key))
      type is (real)
        this%temperature_correct(i) = value
      class default
        this%temperature_correct(i) = real_missing_value
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
      select type (value => hash%specific_humidity_qc%value(level_iterator%key))
      type is (integer)
        this%specific_humidity_qc(i) = value
      class default
        this%specific_humidity_qc(i) = int_missing_value
      end select
      select type (value => hash%specific_humidity_correct%value(level_iterator%key))
      type is (real)
        this%specific_humidity_correct(i) = value
      class default
        this%specific_humidity_correct(i) = real_missing_value
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

  end subroutine raob_profile_set_from_hash

  subroutine raob_profile_final(this)

    type(raob_profile_type), intent(inout) :: this

    if (allocated(this%pressure))                  deallocate(this%pressure)
    if (allocated(this%pressure_qc))               deallocate(this%pressure_qc)
    if (allocated(this%pressure_correct))          deallocate(this%pressure_correct)
    if (allocated(this%height))                    deallocate(this%height)
    if (allocated(this%height_qc))                 deallocate(this%height_qc)
    if (allocated(this%height_correct))            deallocate(this%height_correct)
    if (allocated(this%temperature))               deallocate(this%temperature)
    if (allocated(this%temperature_qc))            deallocate(this%temperature_qc)
    if (allocated(this%temperature_correct))       deallocate(this%temperature_correct)
    if (allocated(this%dewpoint))                  deallocate(this%dewpoint)
    if (allocated(this%specific_humidity))         deallocate(this%specific_humidity)
    if (allocated(this%specific_humidity_qc))      deallocate(this%specific_humidity_qc)
    if (allocated(this%specific_humidity_correct)) deallocate(this%specific_humidity_correct)
    if (allocated(this%relative_humidity))         deallocate(this%relative_humidity)
    if (allocated(this%wind_direction))            deallocate(this%wind_direction)
    if (allocated(this%wind_speed))                deallocate(this%wind_speed)
    if (allocated(this%wind_u))                    deallocate(this%wind_u)
    if (allocated(this%wind_u_correct))            deallocate(this%wind_u_correct)
    if (allocated(this%wind_v))                    deallocate(this%wind_v)
    if (allocated(this%wind_v_correct))            deallocate(this%wind_v_correct)
    if (allocated(this%wind_qc))                   deallocate(this%wind_qc)

  end subroutine raob_profile_final

  subroutine raob_profile_hash_init(this)

    class(raob_profile_hash_type), intent(inout) :: this

    this%pressure                  = hash_table(chunk_size=100, max_load_factor=0.9)
    this%pressure_qc               = hash_table(chunk_size=100, max_load_factor=0.9)
    this%pressure_correct          = hash_table(chunk_size=100, max_load_factor=0.9)
    this%height                    = hash_table(chunk_size=100, max_load_factor=0.9)
    this%height_qc                 = hash_table(chunk_size=100, max_load_factor=0.9)
    this%height_correct            = hash_table(chunk_size=100, max_load_factor=0.9)
    this%temperature               = hash_table(chunk_size=100, max_load_factor=0.9)
    this%temperature_qc            = hash_table(chunk_size=100, max_load_factor=0.9)
    this%temperature_correct       = hash_table(chunk_size=100, max_load_factor=0.9)
    this%dewpoint                  = hash_table(chunk_size=100, max_load_factor=0.9)
    this%specific_humidity         = hash_table(chunk_size=100, max_load_factor=0.9)
    this%specific_humidity_qc      = hash_table(chunk_size=100, max_load_factor=0.9)
    this%specific_humidity_correct = hash_table(chunk_size=100, max_load_factor=0.9)
    this%relative_humidity         = hash_table(chunk_size=100, max_load_factor=0.9)
    this%wind_direction            = hash_table(chunk_size=100, max_load_factor=0.9)
    this%wind_speed                = hash_table(chunk_size=100, max_load_factor=0.9)
    this%wind_u                    = hash_table(chunk_size=100, max_load_factor=0.9)
    this%wind_u_correct            = hash_table(chunk_size=100, max_load_factor=0.9)
    this%wind_v                    = hash_table(chunk_size=100, max_load_factor=0.9)
    this%wind_v_correct            = hash_table(chunk_size=100, max_load_factor=0.9)
    this%wind_qc                   = hash_table(chunk_size=100, max_load_factor=0.9)

  end subroutine raob_profile_hash_init

  subroutine raob_record_init(this, alloc_hash)

    class(raob_record_type), intent(out) :: this
    logical, intent(in), optional :: alloc_hash

    if (present(alloc_hash) .and. alloc_hash) then
      allocate(this%man_hash);  call this%man_hash %init()
      allocate(this%sigt_hash); call this%sigt_hash%init()
      allocate(this%sigw_hash); call this%sigw_hash%init()
      allocate(this%trop_hash); call this%trop_hash%init()
    end if

  end subroutine raob_record_init

  subroutine raob_record_print(this)

    class(raob_record_type), intent(in) :: this

    integer i

    print *, 'Station ', this%station%name
    print *, 'Time ', this%time%isoformat()
    print *, '- Surface:'
    write(*, '(A15, A5)', advance='no') 'P', ''
    write(*, '(A15, A5)', advance='no') 'T', ''
    write(*, '(A15, A5)', advance='no') 'SH', ''
    write(*, '(A15)',     advance='no') 'TD'
    write(*, '(A15, A5)', advance='no') 'U', ''
    write(*, '(A15, A5)', advance='no') 'V', ''
    write(*, '(A15, A5)', advance='no') 'WD', ''
    write(*, '(A15, A5)', advance='no') 'WS', ''
    write(*, *)
    if (is_missing(this%sfc_pressure)) then
      write(*, '(A15)', advance='no') 'X'
      write(*, '(" (", A2, ")")', advance='no') 'X'
    else
      write(*, '(F15.1)', advance='no') this%sfc_pressure
      write(*, '(" (", I2, ")")', advance='no') this%sfc_pressure_qc
    end if
    if (is_missing(this%sfc_temperature)) then
      write(*, '(A15)', advance='no') 'X'
      write(*, '(" (", A2, ")")', advance='no') 'X'
    else
      write(*, '(F15.1)', advance='no') this%sfc_temperature
      write(*, '(" (", I2, ")")', advance='no') this%sfc_temperature_qc
    end if
    if (is_missing(this%sfc_specific_humidity)) then
      write(*, '(A15)', advance='no') 'X'
      write(*, '(" (", A2, ")")', advance='no') 'X'
    else
      write(*, '(F15.1)', advance='no') this%sfc_specific_humidity
      write(*, '(" (", I2, ")")', advance='no') this%sfc_specific_humidity_qc
    end if
    if (is_missing(this%sfc_dewpoint)) then
      write(*, '(A15)', advance='no') 'X'
    else
      write(*, '(F15.1)', advance='no') this%sfc_dewpoint
    end if
    if (is_missing(this%sfc_wind_u)) then
      write(*, '(A15)', advance='no') 'X'
      write(*, '(" (", A2, ")")', advance='no') 'X'
    else
      write(*, '(F15.1)', advance='no') this%sfc_wind_u
      write(*, '(" (", I2, ")")', advance='no') this%sfc_wind_qc
    end if
    if (is_missing(this%sfc_wind_v)) then
      write(*, '(A15)', advance='no') 'X'
      write(*, '(" (", A2, ")")', advance='no') 'X'
    else
      write(*, '(F15.1)', advance='no') this%sfc_wind_v
      write(*, '(" (", I2, ")")', advance='no') this%sfc_wind_qc
    end if
    if (is_missing(this%sfc_wind_direction)) then
      write(*, '(A15)', advance='no') 'X'
      write(*, '(" (", A2, ")")', advance='no') 'X'
    else
      write(*, '(F15.1)', advance='no') this%sfc_wind_direction
      write(*, '(" (", I2, ")")', advance='no') this%sfc_wind_qc
    end if
    if (is_missing(this%sfc_wind_speed)) then
      write(*, '(A15)', advance='no') 'X'
      write(*, '(" (", A2, ")")', advance='no') 'X'
    else
      write(*, '(F15.1)', advance='no') this%sfc_wind_speed
      write(*, '(" (", I2, ")")', advance='no') this%sfc_wind_qc
    end if
    write(*, *)
    print *, '- Mandatory levels:'
    write(*, '(A15, A5)', advance='no') 'P', ''
    write(*, '(A15, A5)', advance='no') 'H', ''
    write(*, '(A15, A5)', advance='no') 'T', ''
    write(*, '(A15, A5)', advance='no') 'SH', ''
    write(*, '(A15)',     advance='no') 'TD'
    write(*, '(A15)',     advance='no') 'RH'
    write(*, '(A15, A5)', advance='no') 'U', ''
    write(*, '(A15, A5)', advance='no') 'V', ''
    write(*, '(A15, A5)', advance='no') 'WD', ''
    write(*, '(A15, A5)', advance='no') 'WS', ''
    write(*, *)
    do i = 1, this%man%num_level
      if (is_missing(this%man%pressure(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%man%pressure(i)
        write(*, '(" (", I2, ")")', advance='no') this%man%pressure_qc(i)
      end if
      if (is_missing(this%man%height(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%man%height(i)
        write(*, '(" (", I2, ")")', advance='no') this%man%height_qc(i)
      end if
      if (is_missing(this%man%temperature(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%man%temperature(i)
        write(*, '(" (", I2, ")")', advance='no') this%man%temperature_qc(i)
      end if
      if (is_missing(this%man%specific_humidity(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%man%specific_humidity(i)
        write(*, '(" (", I2, ")")', advance='no') this%man%specific_humidity_qc(i)
      end if
      if (is_missing(this%man%dewpoint(i))) then
        write(*, '(A15)', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%man%dewpoint(i)
      end if
      if (is_missing(this%man%relative_humidity(i))) then
        write(*, '(A15)', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%man%relative_humidity(i)
      end if
      if (is_missing(this%man%wind_u(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%man%wind_u(i)
        write(*, '(" (", I2, ")")', advance='no') this%man%wind_qc(i)
      end if
      if (is_missing(this%man%wind_v(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%man%wind_v(i)
        write(*, '(" (", I2, ")")', advance='no') this%man%wind_qc(i)
      end if
      if (is_missing(this%man%wind_direction(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%man%wind_direction(i)
        write(*, '(" (", I2, ")")', advance='no') this%man%wind_qc(i)
      end if
      if (is_missing(this%man%wind_speed(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%man%wind_speed(i)
        write(*, '(" (", I2, ")")', advance='no') this%man%wind_qc(i)
      end if
      write(*, *)
    end do
    print *, '- Significant temperature levels:'
    write(*, '(A15, A5)', advance='no') 'P', ''
    write(*, '(A15, A5)', advance='no') 'H', ''
    write(*, '(A15, A5)', advance='no') 'T', ''
    write(*, '(A15, A5)', advance='no') 'SH', ''
    write(*, '(A15)',     advance='no') 'TD'
    write(*, '(A15)',     advance='no') 'RH'
    write(*, '(A15, A5)', advance='no') 'U', ''
    write(*, '(A15, A5)', advance='no') 'V', ''
    write(*, '(A15, A5)', advance='no') 'WD', ''
    write(*, '(A15, A5)', advance='no') 'WS', ''
    write(*, *)
    do i = 1, this%sigt%num_level
      if (is_missing(this%sigt%pressure(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%sigt%pressure(i)
        write(*, '(" (", I2, ")")', advance='no') this%sigt%pressure_qc(i)
      end if
      if (is_missing(this%sigt%height(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%sigt%height(i)
        write(*, '(" (", I2, ")")', advance='no') this%sigt%height_qc(i)
      end if
      if (is_missing(this%sigt%temperature(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%sigt%temperature(i)
        write(*, '(" (", I2, ")")', advance='no') this%sigt%temperature_qc(i)
      end if
      if (is_missing(this%sigt%specific_humidity(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%sigt%specific_humidity(i)
        write(*, '(" (", I2, ")")', advance='no') this%sigt%specific_humidity_qc(i)
      end if
      if (is_missing(this%sigt%dewpoint(i))) then
        write(*, '(A15)', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%sigt%dewpoint(i)
      end if
      if (is_missing(this%sigt%relative_humidity(i))) then
        write(*, '(A15)', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%sigt%relative_humidity(i)
      end if
      if (is_missing(this%sigt%wind_u(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%sigt%wind_u(i)
        write(*, '(" (", I2, ")")', advance='no') this%sigt%wind_qc(i)
      end if
      if (is_missing(this%sigt%wind_v(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%sigt%wind_v(i)
        write(*, '(" (", I2, ")")', advance='no') this%sigt%wind_qc(i)
      end if
      if (is_missing(this%sigt%wind_direction(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%sigt%wind_direction(i)
        write(*, '(" (", I2, ")")', advance='no') this%sigt%wind_qc(i)
      end if
      if (is_missing(this%sigt%wind_speed(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%sigt%wind_speed(i)
        write(*, '(" (", I2, ")")', advance='no') this%sigt%wind_qc(i)
      end if
      write(*, *)
    end do
    print *, '- Significant wind levels:'
    write(*, '(A15, A5)', advance='no') 'P', ''
    write(*, '(A15, A5)', advance='no') 'H', ''
    write(*, '(A15, A5)', advance='no') 'T', ''
    write(*, '(A15, A5)', advance='no') 'SH', ''
    write(*, '(A15)',     advance='no') 'TD'
    write(*, '(A15)',     advance='no') 'RH'
    write(*, '(A15, A5)', advance='no') 'U', ''
    write(*, '(A15, A5)', advance='no') 'V', ''
    write(*, '(A15, A5)', advance='no') 'WD', ''
    write(*, '(A15, A5)', advance='no') 'WS', ''
    write(*, *)
    do i = 1, this%sigw%num_level
      if (is_missing(this%sigw%pressure(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%sigw%pressure(i)
        write(*, '(" (", I2, ")")', advance='no') this%sigw%pressure_qc(i)
      end if
      if (is_missing(this%sigw%height(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%sigw%height(i)
        write(*, '(" (", I2, ")")', advance='no') this%sigw%height_qc(i)
      end if
      if (is_missing(this%sigw%temperature(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%sigw%temperature(i)
        write(*, '(" (", I2, ")")', advance='no') this%sigw%temperature_qc(i)
      end if
      if (is_missing(this%sigw%specific_humidity(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%sigw%specific_humidity(i)
        write(*, '(" (", I2, ")")', advance='no') this%sigw%specific_humidity_qc(i)
      end if
      if (is_missing(this%sigw%dewpoint(i))) then
        write(*, '(A15)', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%sigw%dewpoint(i)
      end if
      if (is_missing(this%sigw%relative_humidity(i))) then
        write(*, '(A15)', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%sigw%relative_humidity(i)
      end if
      if (is_missing(this%sigw%wind_u(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%sigw%wind_u(i)
        write(*, '(" (", I2, ")")', advance='no') this%sigw%wind_qc(i)
      end if
      if (is_missing(this%sigw%wind_v(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%sigw%wind_v(i)
        write(*, '(" (", I2, ")")', advance='no') this%sigw%wind_qc(i)
      end if
      if (is_missing(this%sigw%wind_direction(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%sigw%wind_direction(i)
        write(*, '(" (", I2, ")")', advance='no') this%sigw%wind_qc(i)
      end if
      if (is_missing(this%sigw%wind_speed(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%sigw%wind_speed(i)
        write(*, '(" (", I2, ")")', advance='no') this%sigw%wind_qc(i)
      end if
      write(*, *)
    end do
    print *, '- Tropopause levels:'
    write(*, '(A15, A5)', advance='no') 'P', ''
    write(*, '(A15, A5)', advance='no') 'H', ''
    write(*, '(A15, A5)', advance='no') 'T', ''
    write(*, '(A15, A5)', advance='no') 'SH', ''
    write(*, '(A15)',     advance='no') 'TD'
    write(*, '(A15)',     advance='no') 'RH'
    write(*, '(A15, A5)', advance='no') 'U', ''
    write(*, '(A15, A5)', advance='no') 'V', ''
    write(*, '(A15, A5)', advance='no') 'WD', ''
    write(*, '(A15, A5)', advance='no') 'WS', ''
    write(*, *)
    do i = 1, this%trop%num_level
      if (is_missing(this%trop%pressure(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%trop%pressure(i)
        write(*, '(" (", I2, ")")', advance='no') this%trop%pressure_qc(i)
      end if
      if (is_missing(this%trop%height(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%trop%height(i)
        write(*, '(" (", I2, ")")', advance='no') this%trop%height_qc(i)
      end if
      if (is_missing(this%trop%temperature(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%trop%temperature(i)
        write(*, '(" (", I2, ")")', advance='no') this%trop%temperature_qc(i)
      end if
      if (is_missing(this%trop%specific_humidity(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%trop%specific_humidity(i)
        write(*, '(" (", I2, ")")', advance='no') this%trop%specific_humidity_qc(i)
      end if
      if (is_missing(this%trop%dewpoint(i))) then
        write(*, '(A15)', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%trop%dewpoint(i)
      end if
      if (is_missing(this%trop%relative_humidity(i))) then
        write(*, '(A15)', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%trop%relative_humidity(i)
      end if
      if (is_missing(this%trop%wind_u(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%trop%wind_u(i)
        write(*, '(" (", I2, ")")', advance='no') this%trop%wind_qc(i)
      end if
      if (is_missing(this%trop%wind_v(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%trop%wind_v(i)
        write(*, '(" (", I2, ")")', advance='no') this%trop%wind_qc(i)
      end if
      if (is_missing(this%trop%wind_direction(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%trop%wind_direction(i)
        write(*, '(" (", I2, ")")', advance='no') this%trop%wind_qc(i)
      end if
      if (is_missing(this%trop%wind_speed(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%trop%wind_speed(i)
        write(*, '(" (", I2, ")")', advance='no') this%trop%wind_qc(i)
      end if
      write(*, *)
    end do

  end subroutine raob_record_print

end module raob_mod
