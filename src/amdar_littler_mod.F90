module amdar_littler_mod

  use datetime
  use amdar_mod
  use hash_table_mod
  use linked_list_mod
  use params_mod
  use utils_mod

  implicit none

  private

  public amdar_littler_write

contains

  subroutine amdar_littler_write(file_path)

    character(*), intent(inout) :: file_path

    type(linked_list_iterator_type) record_iterator
    real T, Td, RH
    integer i

    if (file_path == '') file_path = 'amdar.littler'

    open(10, file=file_path, form='formatted')

    i = 1
    record_iterator = linked_list_iterator(records)
    do while (.not. record_iterator%ended())
      select type (record => record_iterator%value)
      type is (amdar_record_type)
        T = add(record%amdar_temperature, freezing_point)
        Td = real_missing_value
        RH = real_missing_value
        ! Header
        write(10, '(F20.5)', advance='no') record%lat                         ! latitude
        write(10, '(F20.5)', advance='no') record%lon                         ! longitude
        write(10, '(A40)',   advance='no') record%flight%name                 ! id
        write(10, '(A40)',   advance='no') record%flight%name                 ! name
        write(10, '(A40)',   advance='no') 'FM-42'                            ! platform
        write(10, '(A40)',   advance='no') 'N/A'                              ! source
        write(10, '(F20.5)', advance='no') record%z                           ! elevation
        write(10, '(I10)',   advance='no') 1                                  ! num_vld_fld
        write(10, '(I10)',   advance='no') 0                                  ! num_error
        write(10, '(I10)',   advance='no') 0                                  ! num_warning
        write(10, '(I10)',   advance='no') i                                  ! seq_num
        write(10, '(I10)',   advance='no') 0                                  ! num_dups
        write(10, '(L10)',   advance='no') .false.                            ! is_sound
        write(10, '(L10)',   advance='no') .false.                            ! bogus
        write(10, '(L10)',   advance='no') .false.                            ! discard
        write(10, '(I10)',   advance='no') int(record%time%timestamp())       ! obs_time
        write(10, '(I10)',   advance='no') record%time%days_in_year()         ! julian_day
        write(10, '(A20)',   advance='no') record%time%format('%Y%m%d%H%M%S') ! date_char
        write(10, '(F13.5)', advance='no') real_missing_value                 ! slp
        write(10, '(I7)',    advance='no') int_missing_value                  ! slp QC
        write(10, '(F13.5)', advance='no') real_missing_value                 ! ref_pres
        write(10, '(I7)',    advance='no') int_missing_value                  ! ref_pres QC
        write(10, '(F13.5)', advance='no') real_missing_value                 ! ground_t
        write(10, '(I7)',    advance='no') int_missing_value                  ! ground_t QC
        write(10, '(F13.5)', advance='no') real_missing_value                 ! SST
        write(10, '(I7)',    advance='no') int_missing_value                  ! SST QC
        write(10, '(F13.5)', advance='no') real_missing_value                 ! psfc
        write(10, '(I7)',    advance='no') int_missing_value                  ! psfc QC
        write(10, '(F13.5)', advance='no') real_missing_value                 ! precip
        write(10, '(I7)',    advance='no') int_missing_value                  ! precip QC
        write(10, '(F13.5)', advance='no') real_missing_value                 ! t_max
        write(10, '(I7)',    advance='no') int_missing_value                  ! t_max QC
        write(10, '(F13.5)', advance='no') real_missing_value                 ! t_min
        write(10, '(I7)',    advance='no') int_missing_value                  ! t_min QC
        write(10, '(F13.5)', advance='no') real_missing_value                 ! t_max night
        write(10, '(I7)',    advance='no') int_missing_value                  ! t_max night QC
        write(10, '(F13.5)', advance='no') real_missing_value                 ! p_tend03
        write(10, '(I7)',    advance='no') int_missing_value                  ! p_tend03 QC
        write(10, '(F13.5)', advance='no') real_missing_value                 ! p_tend24
        write(10, '(I7)',    advance='no') int_missing_value                  ! p_tend24 QC
        write(10, '(F13.5)', advance='no') real_missing_value                 ! clound_cover
        write(10, '(I7)',    advance='no') int_missing_value                  ! clound_cover QC
        write(10, '(F13.5)', advance='no') real_missing_value                 ! ceiling
        write(10, '(I7)',    advance='no') int_missing_value                  ! celing QC
        write(10, *)
        write(10, '(F13.5)', advance='no') record%amdar_pressure              ! pressure (Pa)
        write(10, '(I7)',    advance='no') record%amdar_pressure_qc           ! pressure QC
        write(10, '(F13.5)', advance='no') record%z                           ! height
        write(10, '(I7)',    advance='no') 0                                  ! height QC
        write(10, '(F13.5)', advance='no') T                                  ! temperature (K)
        write(10, '(I7)',    advance='no') record%amdar_temperature_qc        ! temperature QC
        write(10, '(F13.5)', advance='no') Td                                 ! dewpoint (K)
        write(10, '(I7)',    advance='no') 0                                  ! dewpoint QC
        write(10, '(F13.5)', advance='no') record%amdar_wind_speed            ! wind speed (m s^-1)
        write(10, '(I7)',    advance='no') record%amdar_wind_qc               ! wind QC
        write(10, '(F13.5)', advance='no') record%amdar_wind_direction        ! wind direction (degree)
        write(10, '(I7)',    advance='no') record%amdar_wind_qc               ! wind QC
        write(10, '(F13.5)', advance='no') real_missing_value_in_littler      ! wind u component (m s^-1)
        write(10, '(I7)',    advance='no') 0                                  ! wind u component QC
        write(10, '(F13.5)', advance='no') real_missing_value_in_littler      ! wind v component (m s^-1)
        write(10, '(I7)',    advance='no') 0                                  ! wind v component QC
        write(10, '(F13.5)', advance='no') RH                                 ! relative humidity (%)
        write(10, '(I7)',    advance='no') record%amdar_specific_humidity_qc  ! relative humidity QC
        write(10, '(F13.5)', advance='no') real_missing_value_in_littler      ! thickness (m)
        write(10, '(I7)',    advance='no') 0                                  ! thickness QC
        write(10, *)
        write(10, '(F13.5)', advance='no') -777777.0                          ! pressure (Pa)
        write(10, '(I7)',    advance='no') 0                                  ! pressure QC
        write(10, '(F13.5)', advance='no') -777777.0                          ! height
        write(10, '(I7)',    advance='no') 0                                  ! height QC
        write(10, '(F13.5)', advance='no') real_missing_value_in_littler      ! temperature (K)
        write(10, '(I7)',    advance='no') 0                                  ! temperature QC
        write(10, '(F13.5)', advance='no') real_missing_value_in_littler      ! dewpoint (K)
        write(10, '(I7)',    advance='no') 0                                  ! dewpoint QC
        write(10, '(F13.5)', advance='no') real_missing_value_in_littler      ! wind speed (m s^-1)
        write(10, '(I7)',    advance='no') 0                                  ! wind QC
        write(10, '(F13.5)', advance='no') real_missing_value_in_littler      ! wind direction (degree)
        write(10, '(I7)',    advance='no') 0                                  ! wind QC
        write(10, '(F13.5)', advance='no') real_missing_value_in_littler      ! wind u component (m s^-1)
        write(10, '(I7)',    advance='no') 0                                  ! wind u component QC
        write(10, '(F13.5)', advance='no') real_missing_value_in_littler      ! wind v component (m s^-1)
        write(10, '(I7)',    advance='no') 0                                  ! wind v component QC
        write(10, '(F13.5)', advance='no') real_missing_value_in_littler      ! relative humidity (%)
        write(10, '(I7)',    advance='no') 0                                  ! relative humidity QC
        write(10, '(F13.5)', advance='no') real_missing_value_in_littler      ! wind v component (m s^-1)
        write(10, '(I7)',    advance='no') 0                                  ! wind v component QC
        write(10, *)
      end select
      call record_iterator%next()
      i = i + 1
    end do
    write(10, '(3I7)') i, 0, 0

    close(10)

  end subroutine amdar_littler_write

end module amdar_littler_mod
