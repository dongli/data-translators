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
    real, allocatable :: p(:)
    real, allocatable :: h(:)
    real, allocatable :: ta(:)
    real, allocatable :: td(:)
    real, allocatable :: sh(:)
    real, allocatable :: rh(:)
    real, allocatable :: wd(:)
    real, allocatable :: ws(:)
    real, allocatable :: ua(:)
    real, allocatable :: va(:)
    integer, allocatable :: p_qc(:)
    integer, allocatable :: h_qc(:)
    integer, allocatable :: ta_qc(:)
    integer, allocatable :: sh_qc(:)
    integer, allocatable :: wd_qc(:)
    integer, allocatable :: ws_qc(:)
    integer, allocatable :: ua_qc(:)
    integer, allocatable :: va_qc(:)
    real, allocatable :: p_cr(:)
    real, allocatable :: h_cr(:)
    real, allocatable :: ta_cr(:)
    real, allocatable :: sh_cr(:)
    real, allocatable :: ua_cr(:)
    real, allocatable :: va_cr(:)
  contains
    procedure :: init => raob_profile_init
    procedure :: set_from_hash => raob_profile_set_from_hash
    final :: raob_profile_final
  end type raob_profile_type

  type raob_profile_hash_type
    type(hash_table_type) p
    type(hash_table_type) h
    type(hash_table_type) ta
    type(hash_table_type) td
    type(hash_table_type) sh
    type(hash_table_type) rh
    type(hash_table_type) wd
    type(hash_table_type) ws
    type(hash_table_type) ua
    type(hash_table_type) va
    type(hash_table_type) p_qc
    type(hash_table_type) h_qc
    type(hash_table_type) ta_qc
    type(hash_table_type) sh_qc
    type(hash_table_type) wd_qc
    type(hash_table_type) ws_qc
    type(hash_table_type) ua_qc
    type(hash_table_type) va_qc
    type(hash_table_type) p_cr
    type(hash_table_type) h_cr
    type(hash_table_type) ta_cr
    type(hash_table_type) sh_cr
    type(hash_table_type) ua_cr
    type(hash_table_type) va_cr
  contains
    procedure :: init => raob_profile_hash_init
  end type raob_profile_hash_type

  type, extends(obs_static_record_base_type) :: raob_record_type
    type(raob_station_type), pointer :: station
    real    :: ps     = real_missing_value
    real    :: tas    = real_missing_value
    real    :: tds    = real_missing_value
    real    :: shs    = real_missing_value
    real    :: wds    = real_missing_value
    real    :: wss    = real_missing_value
    real    :: uas    = real_missing_value
    real    :: vas    = real_missing_value
    integer :: ps_qc  =  int_missing_value
    integer :: tas_qc =  int_missing_value
    integer :: shs_qc =  int_missing_value
    integer :: wds_qc =  int_missing_value
    integer :: wss_qc =  int_missing_value
    integer :: uas_qc =  int_missing_value
    integer :: vas_qc =  int_missing_value
    real    :: ps_cr  = real_missing_value
    real    :: tas_cr = real_missing_value
    real    :: shs_cr = real_missing_value
    real    :: uas_cr = real_missing_value
    real    :: vas_cr = real_missing_value
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
    allocate(this%p    (num_level))
    allocate(this%p_qc (num_level))
    allocate(this%p_cr (num_level))
    allocate(this%h    (num_level))
    allocate(this%h_qc (num_level))
    allocate(this%h_cr (num_level))
    allocate(this%ta   (num_level))
    allocate(this%ta_qc(num_level))
    allocate(this%ta_cr(num_level))
    allocate(this%td   (num_level))
    allocate(this%sh   (num_level))
    allocate(this%sh_qc(num_level))
    allocate(this%sh_cr(num_level))
    allocate(this%rh   (num_level))
    allocate(this%wd   (num_level))
    allocate(this%wd_qc(num_level))
    allocate(this%ws   (num_level))
    allocate(this%ws_qc(num_level))
    allocate(this%ua   (num_level))
    allocate(this%ua_qc(num_level))
    allocate(this%ua_cr(num_level))
    allocate(this%va   (num_level))
    allocate(this%va_qc(num_level))
    allocate(this%va_cr(num_level))

  end subroutine raob_profile_init

  subroutine raob_profile_set_from_hash(this, hash)

    class(raob_profile_type), intent(inout) :: this
    type(raob_profile_hash_type), intent(inout), pointer :: hash

    integer i
    type(hash_table_iterator_type) level_iterator

    i = 1
    level_iterator = hash_table_iterator(hash%p)
    do while (.not. level_iterator%ended())
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
      ! temperature (degC)
      select type (value => hash%ta%value(level_iterator%key))
      type is (real)
        this%ta(i) = value
      class default
        this%ta(i) = real_missing_value
      end select
      select type (value => hash%ta_qc%value(level_iterator%key))
      type is (integer)
        this%ta_qc(i) = value
      class default
        this%ta_qc(i) = int_missing_value
      end select
      select type (value => hash%ta_cr%value(level_iterator%key))
      type is (real)
        this%ta_cr(i) = value
      class default
        this%ta_cr(i) = real_missing_value
      end select
      ! dewpoint (degC)
      select type (value => hash%td%value(level_iterator%key))
      type is (real)
        this%td(i) = value
      class default
        this%td(i) = real_missing_value
      end select
      ! specific humidity (Mg/Kg)
      select type (value => hash%sh%value(level_iterator%key))
      type is (real)
        this%sh(i) = value
      class default
        this%sh(i) = real_missing_value
      end select
      select type (value => hash%sh_qc%value(level_iterator%key))
      type is (integer)
        this%sh_qc(i) = value
      class default
        this%sh_qc(i) = int_missing_value
      end select
      select type (value => hash%sh_cr%value(level_iterator%key))
      type is (real)
        this%sh_cr(i) = value
      class default
        this%sh_cr(i) = real_missing_value
      end select
      ! relative humidity (%)
      select type (value => hash%rh%value(level_iterator%key))
      type is (real)
        this%rh(i) = value
      class default
        this%rh(i) = real_missing_value
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
      i = i + 1
      call level_iterator%next()
    end do

    ! Release memory of hash.
    deallocate(hash)

  end subroutine raob_profile_set_from_hash

  subroutine raob_profile_final(this)

    type(raob_profile_type), intent(inout) :: this

    if (allocated(this%p    )) deallocate(this%p    )
    if (allocated(this%p_qc )) deallocate(this%p_qc )
    if (allocated(this%p_cr )) deallocate(this%p_cr )
    if (allocated(this%h    )) deallocate(this%h    )
    if (allocated(this%h_qc )) deallocate(this%h_qc )
    if (allocated(this%h_cr )) deallocate(this%h_cr )
    if (allocated(this%ta   )) deallocate(this%ta   )
    if (allocated(this%ta_qc)) deallocate(this%ta_qc)
    if (allocated(this%ta_cr)) deallocate(this%ta_cr)
    if (allocated(this%td   )) deallocate(this%td   )
    if (allocated(this%sh   )) deallocate(this%sh   )
    if (allocated(this%sh_qc)) deallocate(this%sh_qc)
    if (allocated(this%sh_cr)) deallocate(this%sh_cr)
    if (allocated(this%rh   )) deallocate(this%rh   )
    if (allocated(this%wd   )) deallocate(this%wd   )
    if (allocated(this%wd_qc)) deallocate(this%wd_qc)
    if (allocated(this%ws   )) deallocate(this%ws   )
    if (allocated(this%ws_qc)) deallocate(this%ws_qc)
    if (allocated(this%ua   )) deallocate(this%ua   )
    if (allocated(this%ua_qc)) deallocate(this%ua_qc)
    if (allocated(this%ua_cr)) deallocate(this%ua_cr)
    if (allocated(this%va   )) deallocate(this%va   )
    if (allocated(this%va_qc)) deallocate(this%va_qc)
    if (allocated(this%va_cr)) deallocate(this%va_cr)

  end subroutine raob_profile_final

  subroutine raob_profile_hash_init(this)

    class(raob_profile_hash_type), intent(inout) :: this

    this%p     = hash_table(chunk_size=100, max_load_factor=0.9)
    this%p_qc  = hash_table(chunk_size=100, max_load_factor=0.9)
    this%p_cr  = hash_table(chunk_size=100, max_load_factor=0.9)
    this%h     = hash_table(chunk_size=100, max_load_factor=0.9)
    this%h_qc  = hash_table(chunk_size=100, max_load_factor=0.9)
    this%h_cr  = hash_table(chunk_size=100, max_load_factor=0.9)
    this%ta    = hash_table(chunk_size=100, max_load_factor=0.9)
    this%ta_qc = hash_table(chunk_size=100, max_load_factor=0.9)
    this%ta_cr = hash_table(chunk_size=100, max_load_factor=0.9)
    this%td    = hash_table(chunk_size=100, max_load_factor=0.9)
    this%sh    = hash_table(chunk_size=100, max_load_factor=0.9)
    this%sh_qc = hash_table(chunk_size=100, max_load_factor=0.9)
    this%sh_cr = hash_table(chunk_size=100, max_load_factor=0.9)
    this%rh    = hash_table(chunk_size=100, max_load_factor=0.9)
    this%wd    = hash_table(chunk_size=100, max_load_factor=0.9)
    this%wd_qc = hash_table(chunk_size=100, max_load_factor=0.9)
    this%ws    = hash_table(chunk_size=100, max_load_factor=0.9)
    this%ws_qc = hash_table(chunk_size=100, max_load_factor=0.9)
    this%ua    = hash_table(chunk_size=100, max_load_factor=0.9)
    this%ua_qc = hash_table(chunk_size=100, max_load_factor=0.9)
    this%ua_cr = hash_table(chunk_size=100, max_load_factor=0.9)
    this%va    = hash_table(chunk_size=100, max_load_factor=0.9)
    this%va_qc = hash_table(chunk_size=100, max_load_factor=0.9)
    this%va_cr = hash_table(chunk_size=100, max_load_factor=0.9)

  end subroutine raob_profile_hash_init

  subroutine raob_record_init(this, alloc_hash)

    class(raob_record_type), intent(out) :: this
    logical, intent(in), optional :: alloc_hash

    if (present(alloc_hash) .and. alloc_hash) then
      allocate(this%man_hash ); call this%man_hash %init()
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
    if (is_missing(this%ps)) then
      write(*, '(A15)', advance='no') 'X'
      write(*, '(" (", A2, ")")', advance='no') 'X'
    else
      write(*, '(F15.1)', advance='no') this%ps
      write(*, '(" (", I2, ")")', advance='no') this%ps_qc
    end if
    if (is_missing(this%tas)) then
      write(*, '(A15)', advance='no') 'X'
      write(*, '(" (", A2, ")")', advance='no') 'X'
    else
      write(*, '(F15.1)', advance='no') this%tas
      write(*, '(" (", I2, ")")', advance='no') this%tas_qc
    end if
    if (is_missing(this%shs)) then
      write(*, '(A15)', advance='no') 'X'
      write(*, '(" (", A2, ")")', advance='no') 'X'
    else
      write(*, '(F15.1)', advance='no') this%shs
      write(*, '(" (", I2, ")")', advance='no') this%shs_qc
    end if
    if (is_missing(this%tds)) then
      write(*, '(A15)', advance='no') 'X'
    else
      write(*, '(F15.1)', advance='no') this%tds
    end if
    if (is_missing(this%uas)) then
      write(*, '(A15)', advance='no') 'X'
      write(*, '(" (", A2, ")")', advance='no') 'X'
    else
      write(*, '(F15.1)', advance='no') this%uas
      write(*, '(" (", I2, ")")', advance='no') this%uas_qc
    end if
    if (is_missing(this%vas)) then
      write(*, '(A15)', advance='no') 'X'
      write(*, '(" (", A2, ")")', advance='no') 'X'
    else
      write(*, '(F15.1)', advance='no') this%vas
      write(*, '(" (", I2, ")")', advance='no') this%vas_qc
    end if
    if (is_missing(this%wds)) then
      write(*, '(A15)', advance='no') 'X'
      write(*, '(" (", A2, ")")', advance='no') 'X'
    else
      write(*, '(F15.1)', advance='no') this%wds
      write(*, '(" (", I2, ")")', advance='no') this%wds_qc
    end if
    if (is_missing(this%wss)) then
      write(*, '(A15)', advance='no') 'X'
      write(*, '(" (", A2, ")")', advance='no') 'X'
    else
      write(*, '(F15.1)', advance='no') this%wss
      write(*, '(" (", I2, ")")', advance='no') this%wss_qc
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
      if (is_missing(this%man%p(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%man%p(i)
        write(*, '(" (", I2, ")")', advance='no') this%man%p_qc(i)
      end if
      if (is_missing(this%man%h(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%man%h(i)
        write(*, '(" (", I2, ")")', advance='no') this%man%h_qc(i)
      end if
      if (is_missing(this%man%ta(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%man%ta(i)
        write(*, '(" (", I2, ")")', advance='no') this%man%ta_qc(i)
      end if
      if (is_missing(this%man%sh(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%man%sh(i)
        write(*, '(" (", I2, ")")', advance='no') this%man%sh_qc(i)
      end if
      if (is_missing(this%man%td(i))) then
        write(*, '(A15)', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%man%td(i)
      end if
      if (is_missing(this%man%rh(i))) then
        write(*, '(A15)', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%man%rh(i)
      end if
      if (is_missing(this%man%ua(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%man%ua(i)
        write(*, '(" (", I2, ")")', advance='no') this%man%ua_qc(i)
      end if
      if (is_missing(this%man%va(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%man%va(i)
        write(*, '(" (", I2, ")")', advance='no') this%man%va_qc(i)
      end if
      if (is_missing(this%man%wd(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%man%wd(i)
        write(*, '(" (", I2, ")")', advance='no') this%man%wd_qc(i)
      end if
      if (is_missing(this%man%ws(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%man%ws(i)
        write(*, '(" (", I2, ")")', advance='no') this%man%ws_qc(i)
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
      if (is_missing(this%sigt%p(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%sigt%p(i)
        write(*, '(" (", I2, ")")', advance='no') this%sigt%p_qc(i)
      end if
      if (is_missing(this%sigt%h(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%sigt%h(i)
        write(*, '(" (", I2, ")")', advance='no') this%sigt%h_qc(i)
      end if
      if (is_missing(this%sigt%ta(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%sigt%ta(i)
        write(*, '(" (", I2, ")")', advance='no') this%sigt%ta_qc(i)
      end if
      if (is_missing(this%sigt%sh(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%sigt%sh(i)
        write(*, '(" (", I2, ")")', advance='no') this%sigt%sh_qc(i)
      end if
      if (is_missing(this%sigt%td(i))) then
        write(*, '(A15)', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%sigt%td(i)
      end if
      if (is_missing(this%sigt%rh(i))) then
        write(*, '(A15)', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%sigt%rh(i)
      end if
      if (is_missing(this%sigt%ua(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%sigt%ua(i)
        write(*, '(" (", I2, ")")', advance='no') this%sigt%ua_qc(i)
      end if
      if (is_missing(this%sigt%va(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%sigt%va(i)
        write(*, '(" (", I2, ")")', advance='no') this%sigt%va_qc(i)
      end if
      if (is_missing(this%sigt%wd(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%sigt%wd(i)
        write(*, '(" (", I2, ")")', advance='no') this%sigt%wd_qc(i)
      end if
      if (is_missing(this%sigt%ws(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%sigt%ws(i)
        write(*, '(" (", I2, ")")', advance='no') this%sigt%ws_qc(i)
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
      if (is_missing(this%sigw%p(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%sigw%p(i)
        write(*, '(" (", I2, ")")', advance='no') this%sigw%p_qc(i)
      end if
      if (is_missing(this%sigw%h(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%sigw%h(i)
        write(*, '(" (", I2, ")")', advance='no') this%sigw%h_qc(i)
      end if
      if (is_missing(this%sigw%ta(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%sigw%ta(i)
        write(*, '(" (", I2, ")")', advance='no') this%sigw%ta_qc(i)
      end if
      if (is_missing(this%sigw%sh(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%sigw%sh(i)
        write(*, '(" (", I2, ")")', advance='no') this%sigw%sh_qc(i)
      end if
      if (is_missing(this%sigw%td(i))) then
        write(*, '(A15)', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%sigw%td(i)
      end if
      if (is_missing(this%sigw%rh(i))) then
        write(*, '(A15)', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%sigw%rh(i)
      end if
      if (is_missing(this%sigw%ua(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%sigw%ua(i)
        write(*, '(" (", I2, ")")', advance='no') this%sigw%ua_qc(i)
      end if
      if (is_missing(this%sigw%va(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%sigw%va(i)
        write(*, '(" (", I2, ")")', advance='no') this%sigw%va_qc(i)
      end if
      if (is_missing(this%sigw%wd(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%sigw%wd(i)
        write(*, '(" (", I2, ")")', advance='no') this%sigw%wd_qc(i)
      end if
      if (is_missing(this%sigw%ws(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%sigw%ws(i)
        write(*, '(" (", I2, ")")', advance='no') this%sigw%ws_qc(i)
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
      if (is_missing(this%trop%p(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%trop%p(i)
        write(*, '(" (", I2, ")")', advance='no') this%trop%p_qc(i)
      end if
      if (is_missing(this%trop%h(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%trop%h(i)
        write(*, '(" (", I2, ")")', advance='no') this%trop%h_qc(i)
      end if
      if (is_missing(this%trop%ta(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%trop%ta(i)
        write(*, '(" (", I2, ")")', advance='no') this%trop%ta_qc(i)
      end if
      if (is_missing(this%trop%sh(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%trop%sh(i)
        write(*, '(" (", I2, ")")', advance='no') this%trop%sh_qc(i)
      end if
      if (is_missing(this%trop%td(i))) then
        write(*, '(A15)', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%trop%td(i)
      end if
      if (is_missing(this%trop%rh(i))) then
        write(*, '(A15)', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%trop%rh(i)
      end if
      if (is_missing(this%trop%ua(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%trop%ua(i)
        write(*, '(" (", I2, ")")', advance='no') this%trop%ua_qc(i)
      end if
      if (is_missing(this%trop%va(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%trop%va(i)
        write(*, '(" (", I2, ")")', advance='no') this%trop%va_qc(i)
      end if
      if (is_missing(this%trop%wd(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%trop%wd(i)
        write(*, '(" (", I2, ")")', advance='no') this%trop%wd_qc(i)
      end if
      if (is_missing(this%trop%ws(i))) then
        write(*, '(A15)', advance='no') 'X'
        write(*, '(" (", A2, ")")', advance='no') 'X'
      else
        write(*, '(F15.1)', advance='no') this%trop%ws(i)
        write(*, '(" (", I2, ")")', advance='no') this%trop%ws_qc(i)
      end if
      write(*, *)
    end do

  end subroutine raob_record_print

end module raob_mod
