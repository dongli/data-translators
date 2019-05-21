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
    real    :: pressure             = real_missing_value ! Pressure (Pa)
    real    :: air_temperature      = real_missing_value ! Air temperature (degC)
    real    :: sea_temperature      = real_missing_value ! Sea temperature / SST (degC)
    real    :: dewpoint             = real_missing_value ! Dewpoint temperature (degC)
    real    :: relative_humidity    = real_missing_value ! Relative humidity (%)
    real    :: specific_humidity    = real_missing_value ! Specific humidity (Mg/Kg)
    real    :: wind_speed           = real_missing_value ! Wind speed (m/s)
    real    :: wind_direction       = real_missing_value ! Wind direction (deg)
    real    :: wind_u               = real_missing_value ! U wind component (m/s)
    real    :: wind_v               = real_missing_value ! V wind component (m/s)
    real    :: wind_wave_height     = real_missing_value ! Wind wave height (m)
    real    :: wind_wave_period     = real_missing_value ! Wind wave period (s)
    real    :: surge_wave_height    = real_missing_value ! Surge wave height (m)
    real    :: surge_wave_period    = real_missing_value ! Surge wave period (s)
    real    :: surge_wave_direction = real_missing_value ! Surge wave direction (deg)
    real    :: visibility           = real_missing_value ! Visibility (m)
    real    :: cloud_cover          = real_missing_value ! Cloud cover (%)
    real    :: ice_cover            = real_missing_value

    integer :: pressure_qc             = int_missing_value
    integer :: air_temperature_qc      = int_missing_value
    integer :: sea_temperature_qc      = int_missing_value
    integer :: dewpoint_qc             = int_missing_value
    integer :: relative_humidity_qc    = int_missing_value
    integer :: specific_humidity_qc    = int_missing_value
    integer :: wind_qc                 = int_missing_value
    integer :: wind_wave_height_qc     = int_missing_value
    integer :: wind_wave_period_qc     = int_missing_value
    integer :: surge_wave_height_qc    = int_missing_value
    integer :: surge_wave_period_qc    = int_missing_value
    integer :: surge_wave_direction_qc = int_missing_value
    integer :: visibility_qc           = int_missing_value
    integer :: cloud_cover_qc          = int_missing_value
    integer :: ice_cover_qc            = int_missing_value
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
    print *, 'p:   ', record%pressure, record%pressure_qc
    print *, 'Ta:  ', record%air_temperature, record%air_temperature_qc
    print *, 'Td:  ', record%dewpoint, record%dewpoint_qc
    print *, 'SST: ', record%sea_temperature, record%sea_temperature_qc
    print *, 'RH:  ', record%relative_humidity, record%relative_humidity_qc
    print *, 'WS:  ', record%wind_speed, record%wind_qc
    print *, 'WD:  ', record%wind_direction, record%wind_qc

  end subroutine ship_record_print

end module ship_mod
