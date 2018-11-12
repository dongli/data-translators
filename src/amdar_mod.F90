module amdar_mod

  use obs_base_mod
  use params_mod
  use hash_table_mod
  use linked_list_mod

  implicit none

  type, extends(obs_site_nopos_base_type) :: amdar_flight_type
    character(8) number
    type(linked_list_type), pointer :: records => null()
  contains
    procedure :: init => amdar_flight_init
    final :: amdar_flight_final
  end type amdar_flight_type

  type, extends(obs_drift_record_base_type) :: amdar_record_type
    type(amdar_flight_type), pointer :: flight
    real    :: amdar_pressure          = real_missing_value ! Pressure (Pa)
    real    :: amdar_height            = real_missing_value ! Height (m)
    real    :: amdar_temperature       = real_missing_value ! Temperature (degC)
    real    :: amdar_specific_humidity = real_missing_value ! Specific humidity (Mg/Kg)
    real    :: amdar_dewpoint          = real_missing_value ! Dewpoint temperature (degC)
    real    :: amdar_relative_humidity = real_missing_value ! Relative humidity (%)
    real    :: amdar_wind_speed        = real_missing_value ! Wind speed (m/s)
    real    :: amdar_wind_direction    = real_missing_value ! Wind direction (deg)
    real    :: amdar_wind_u            = real_missing_value ! U wind component (m/s)
    real    :: amdar_wind_v            = real_missing_value ! V wind component (m/s)
    integer :: amdar_turbulence_index  = int_missing_value  ! Turbulence index

    integer :: amdar_pressure_qc          = int_missing_value
    integer :: amdar_height_qc            = int_missing_value
    integer :: amdar_temperature_qc       = int_missing_value
    integer :: amdar_dewpoint_qc          = int_missing_value
    integer :: amdar_specific_humidity_qc = int_missing_value
    integer :: amdar_relative_humidity_qc = int_missing_value
    integer :: amdar_wind_qc              = int_missing_value
  end type amdar_record_type

contains

  subroutine amdar_flight_init(this, name)

    class(amdar_flight_type), intent(inout) :: this
    character(*), intent(in) :: name

    this%name = name
    if (.not. associated(this%records)) allocate(this%records)

  end subroutine amdar_flight_init

  subroutine amdar_flight_final(this)

    type(amdar_flight_type), intent(inout) :: this

    if (associated(this%records)) deallocate(this%records)

  end subroutine amdar_flight_final

end module amdar_mod
