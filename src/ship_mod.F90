module ship_mod

  use obs_base_mod
  use params_mod
  use hash_table_mod
  use linked_list_mod

  implicit none

  type, extends(obs_site_nopos_base_type) :: ship_type
    type(linked_list_type), pointer :: records => null()
  contains
    procedure :: init => ship_init
    final :: ship_final
  end type ship_type

  type, extends(obs_drift_record_base_type) :: ship_record_type
    type(ship_type), pointer :: ship
    real    :: ps  = real_missing_value ! Pressure (hPa)
    real    :: ta  = real_missing_value ! Air temperature (degC)
    real    :: sst = real_missing_value ! Sea temperature / SST (degC)
    real    :: td  = real_missing_value ! Dewpoint temperature (degC)
    real    :: rh  = real_missing_value ! Relative humidity (%)
    real    :: sh  = real_missing_value ! Specific humidity (Mg/Kg)
    real    :: ws  = real_missing_value ! Wind speed (m/s)
    real    :: wd  = real_missing_value ! Wind direction (deg)
    real    :: ua  = real_missing_value ! U wind component (m/s)
    real    :: va  = real_missing_value ! V wind component (m/s)
    real    :: hww = real_missing_value ! Wind wave height (m)
    real    :: pww = real_missing_value ! Wind wave period (s)
    real    :: hsw = real_missing_value ! Surge wave height (m)
    real    :: psw = real_missing_value ! Surge wave period (s)
    real    :: dsw = real_missing_value ! Surge wave direction (deg)
    real    :: vis = real_missing_value ! Visibility (m)
    real    :: clc = real_missing_value ! Cloud cover (%)
    real    :: ice = real_missing_value ! Ice cover (%)

    integer :: p_qc   = int_missing_value
    integer :: ta_qc  = int_missing_value
    integer :: sst_qc = int_missing_value
    integer :: td_qc  = int_missing_value
    integer :: rh_qc  = int_missing_value
    integer :: sh_qc  = int_missing_value
    integer :: ua_qc  = int_missing_value
    integer :: va_qc  = int_missing_value
    integer :: wd_qc  = int_missing_value
    integer :: ws_qc  = int_missing_value
    integer :: hww_qc = int_missing_value
    integer :: pww_qc = int_missing_value
    integer :: hsw_qc = int_missing_value
    integer :: psw_qc = int_missing_value
    integer :: dsw_qc = int_missing_value
    integer :: vis_qc = int_missing_value
    integer :: clc_qc = int_missing_value
    integer :: ice_qc = int_missing_value
  contains
    procedure :: print => ship_record_print
  end type ship_record_type

contains

  subroutine ship_init(this, name)

    class(ship_type), intent(inout) :: this
    character(*), intent(in) :: name

    this%name = name
    if (.not. associated(this%records)) allocate(this%records)

  end subroutine ship_init

  subroutine ship_final(this)

    type(ship_type), intent(inout) :: this

    if (associated(this%records)) deallocate(this%records)

  end subroutine ship_final

  subroutine ship_record_print(record)

    class(ship_record_type), intent(in) :: record

    print *, '--'
    print *, record%ship%name, record%time%isoformat()
    print *, 'lon: ', record%lon
    print *, 'lat: ', record%lat
    print *, 'p:   ', record%p  , record%p_qc
    print *, 'Ta:  ', record%ta , record%ta_qc
    print *, 'Td:  ', record%td , record%td_qc
    print *, 'SST: ', record%sst, record%sst_qc
    print *, 'RH:  ', record%rh , record%rh_qc
    print *, 'WS:  ', record%ws , record%ws_qc
    print *, 'WD:  ', record%wd , record%wd_qc

  end subroutine ship_record_print

end module ship_mod
