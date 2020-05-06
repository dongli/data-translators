module anem_nrg_mod

  use container
  use obs_base_mod
  use params_mod

  implicit none

  type, extends(obs_station_type) :: anem_nrg_tower_type
    type(linked_list_type), pointer :: records => null()
  contains
    procedure :: init => anem_nrg_tower_init
    final :: anem_nrg_tower_final
  end type anem_nrg_tower_type

  type, extends(obs_static_record_base_type) :: anem_nrg_record_type
    type(anem_nrg_tower_type), pointer :: tower
    real, allocatable :: h(:)
    real, allocatable :: wd_avg(:) ! Wind direction (deg)
    real, allocatable :: wd_std(:)
    real, allocatable :: wd_min(:)
    real, allocatable :: wd_max(:)
    real, allocatable :: ws_avg(:) ! Wind speed (m s-1)
    real, allocatable :: ws_std(:)
    real, allocatable :: ws_min(:)
    real, allocatable :: ws_max(:)
    real, allocatable :: ua_avg(:) ! Wind u component (m s-1)
    real, allocatable :: va_avg(:) ! Wind v component (m s-1)
    real, allocatable :: ta_avg(:) ! Temperature (degC)
    real, allocatable :: ta_std(:)
    real, allocatable :: ta_min(:)
    real, allocatable :: ta_max(:)
    real, allocatable :: p_avg(:)  ! Pressure (hPa)
    real, allocatable :: p_std(:)
    real, allocatable :: p_min(:)
    real, allocatable :: p_max(:)
    real, allocatable :: rh(:)     ! Relative humidity (%)
  contains
    procedure :: init => anem_nrg_record_init
    final :: anem_nrg_record_final
  end type anem_nrg_record_type

contains

  subroutine anem_nrg_tower_init(this, name, lon, lat, z)

    class(anem_nrg_tower_type), intent(inout) :: this
    character(*), intent(in) :: name
    real, intent(in) :: lon
    real, intent(in) :: lat
    real, intent(in) :: z

    this%name = name
    this%lon = lon
    this%lat = lat
    this%z = z
    if (.not. associated(this%records)) allocate(this%records)

  end subroutine anem_nrg_tower_init

  subroutine anem_nrg_tower_final(this)

    type(anem_nrg_tower_type), intent(inout) :: this

    if (associated(this%records)) deallocate(this%records)

  end subroutine anem_nrg_tower_final

  subroutine anem_nrg_record_init(this, num_level)

    class(anem_nrg_record_type), intent(inout) :: this
    integer, intent(in) :: num_level

    if (allocated(this%h     )) deallocate(this%h     )
    if (allocated(this%wd_avg)) deallocate(this%wd_avg)
    if (allocated(this%wd_std)) deallocate(this%wd_std)
    if (allocated(this%wd_min)) deallocate(this%wd_min)
    if (allocated(this%wd_max)) deallocate(this%wd_max)
    if (allocated(this%ws_avg)) deallocate(this%ws_avg)
    if (allocated(this%ws_std)) deallocate(this%ws_std)
    if (allocated(this%ws_min)) deallocate(this%ws_min)
    if (allocated(this%ws_max)) deallocate(this%ws_max)
    if (allocated(this%ua_avg)) deallocate(this%ua_avg)
    if (allocated(this%va_avg)) deallocate(this%va_avg)
    if (allocated(this%ta_avg)) deallocate(this%ta_avg)
    if (allocated(this%ta_std)) deallocate(this%ta_std)
    if (allocated(this%ta_min)) deallocate(this%ta_min)
    if (allocated(this%ta_max)) deallocate(this%ta_max)
    if (allocated(this%p_avg )) deallocate(this%p_avg )
    if (allocated(this%p_std )) deallocate(this%p_std )
    if (allocated(this%p_min )) deallocate(this%p_min )
    if (allocated(this%p_max )) deallocate(this%p_max )

    allocate(this%h     (num_level)); this%h      = real_missing_value
    allocate(this%wd_avg(num_level)); this%wd_avg = real_missing_value
    allocate(this%wd_std(num_level)); this%wd_std = real_missing_value
    allocate(this%wd_min(num_level)); this%wd_min = real_missing_value
    allocate(this%wd_max(num_level)); this%wd_max = real_missing_value
    allocate(this%ws_avg(num_level)); this%ws_avg = real_missing_value
    allocate(this%ws_std(num_level)); this%ws_std = real_missing_value
    allocate(this%ws_min(num_level)); this%ws_min = real_missing_value
    allocate(this%ws_max(num_level)); this%ws_max = real_missing_value
    allocate(this%ua_avg(num_level)); this%ua_avg = real_missing_value
    allocate(this%va_avg(num_level)); this%va_avg = real_missing_value
    allocate(this%ta_avg(num_level)); this%ta_avg = real_missing_value
    allocate(this%ta_std(num_level)); this%ta_std = real_missing_value
    allocate(this%ta_min(num_level)); this%ta_min = real_missing_value
    allocate(this%ta_max(num_level)); this%ta_max = real_missing_value
    allocate(this%p_avg (num_level)); this%p_avg  = real_missing_value
    allocate(this%p_std (num_level)); this%p_std  = real_missing_value
    allocate(this%p_min (num_level)); this%p_min  = real_missing_value
    allocate(this%p_max (num_level)); this%p_max  = real_missing_value

  end subroutine anem_nrg_record_init

  subroutine anem_nrg_record_final(this)

    type(anem_nrg_record_type), intent(inout) :: this

    if (allocated(this%h     )) deallocate(this%h     )
    if (allocated(this%wd_avg)) deallocate(this%wd_avg)
    if (allocated(this%wd_std)) deallocate(this%wd_std)
    if (allocated(this%wd_min)) deallocate(this%wd_min)
    if (allocated(this%wd_max)) deallocate(this%wd_max)
    if (allocated(this%ws_avg)) deallocate(this%ws_avg)
    if (allocated(this%ws_std)) deallocate(this%ws_std)
    if (allocated(this%ws_min)) deallocate(this%ws_min)
    if (allocated(this%ws_max)) deallocate(this%ws_max)
    if (allocated(this%ua_avg)) deallocate(this%ua_avg)
    if (allocated(this%va_avg)) deallocate(this%va_avg)
    if (allocated(this%ta_avg)) deallocate(this%ta_avg)
    if (allocated(this%ta_std)) deallocate(this%ta_std)
    if (allocated(this%ta_min)) deallocate(this%ta_min)
    if (allocated(this%ta_max)) deallocate(this%ta_max)
    if (allocated(this%p_avg )) deallocate(this%p_avg )
    if (allocated(this%p_std )) deallocate(this%p_std )
    if (allocated(this%p_min )) deallocate(this%p_min )
    if (allocated(this%p_max )) deallocate(this%p_max )

  end subroutine anem_nrg_record_final

end module anem_nrg_mod
