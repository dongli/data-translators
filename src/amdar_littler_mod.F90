module amdar_littler_mod

  use datetime
  use string
  use amdar_mod
  use hash_table_mod
  use linked_list_mod
  use params_mod
  use utils_mod

  implicit none

  private

  public amdar_littler_write

contains

  subroutine amdar_littler_write(file_path, flights, records)

    character(*), intent(inout) :: file_path
    type(hash_table_type), intent(inout) :: flights
    type(linked_list_type), intent(inout) :: records

    type(linked_list_iterator_type) record_iterator
    real T, Td
    integer i

    if (file_path == '') file_path = 'amdar.littler'

    open(10, file=file_path, form='formatted')

    i = 1
    record_iterator = linked_list_iterator(records)
    do while (.not. record_iterator%ended())
      select type (record => record_iterator%value)
      type is (amdar_record_type)
        T = add(record%ta, freezing_point)
        Td = add(record%td, freezing_point)
        ! Header
        write(10, '(F20.5)', advance='no') littler_value(record%lat)          ! latitude
        write(10, '(F20.5)', advance='no') littler_value(record%lon)          ! longitude
        write(10, '(A40)',   advance='no') pad_string(record%flight%name, 40) ! id
        write(10, '(A40)',   advance='no') pad_string(record%flight%name, 40) ! name
        write(10, '(A40)',   advance='no') pad_string('FM-42 AMDAR', 40)      ! platform
        write(10, '(A40)',   advance='no') pad_string('N/A', 40)              ! source
        write(10, '(F20.5)', advance='no') littler_value(record%z)            ! elevation
        write(10, '(I10)',   advance='no') 1                                  ! num_vld_fld
        write(10, '(I10)',   advance='no') 0                                  ! num_error
        write(10, '(I10)',   advance='no') 0                                  ! num_warning
        write(10, '(I10)',   advance='no') i                                  ! seq_num
        write(10, '(I10)',   advance='no') 0                                  ! num_dups
        write(10, '(L10)',   advance='no') .false.                            ! is_sound
        write(10, '(L10)',   advance='no') .false.                            ! bogus
        write(10, '(L10)',   advance='no') .false.                            ! discard
        write(10, '(I10)',   advance='no') int_missing_value_in_littler       ! obs_time
        write(10, '(I10)',   advance='no') int_missing_value_in_littler       ! julian_day
        write(10, '(A20)',   advance='no') adjustr(pad_string(record%time%format('%Y%m%d%H%M%S'), 20)) ! date_char
        write(10, '(F13.5)', advance='no') real_missing_value_in_littler      ! slp
        write(10, '(I7)',    advance='no') 0                                  ! slp QC
        write(10, '(F13.5)', advance='no') real_missing_value_in_littler      ! ref_pres
        write(10, '(I7)',    advance='no') 0                                  ! ref_pres QC
        write(10, '(F13.5)', advance='no') real_missing_value_in_littler      ! ground_t
        write(10, '(I7)',    advance='no') 0                                  ! ground_t QC
        write(10, '(F13.5)', advance='no') real_missing_value_in_littler      ! SST
        write(10, '(I7)',    advance='no') 0                                  ! SST QC
        write(10, '(F13.5)', advance='no') real_missing_value_in_littler      ! psfc
        write(10, '(I7)',    advance='no') 0                                  ! psfc QC
        write(10, '(F13.5)', advance='no') real_missing_value_in_littler      ! precip
        write(10, '(I7)',    advance='no') 0                                  ! precip QC
        write(10, '(F13.5)', advance='no') real_missing_value_in_littler      ! t_max
        write(10, '(I7)',    advance='no') 0                                  ! t_max QC
        write(10, '(F13.5)', advance='no') real_missing_value_in_littler      ! t_min
        write(10, '(I7)',    advance='no') 0                                  ! t_min QC
        write(10, '(F13.5)', advance='no') real_missing_value_in_littler      ! t_max night
        write(10, '(I7)',    advance='no') 0                                  ! t_max night QC
        write(10, '(F13.5)', advance='no') real_missing_value_in_littler      ! p_tend03
        write(10, '(I7)',    advance='no') 0                                  ! p_tend03 QC
        write(10, '(F13.5)', advance='no') real_missing_value_in_littler      ! p_tend24
        write(10, '(I7)',    advance='no') 0                                  ! p_tend24 QC
        write(10, '(F13.5)', advance='no') real_missing_value_in_littler      ! clound_cover
        write(10, '(I7)',    advance='no') 0                                  ! clound_cover QC
        write(10, '(F13.5)', advance='no') real_missing_value_in_littler      ! ceiling
        write(10, '(I7)',    advance='no') 0                                  ! celing QC
        write(10, *)
        write(10, '(F13.5)', advance='no') littler_value(record%p)            ! pressure (Pa)
        write(10, '(I7)',    advance='no') 0                                  ! pressure QC
        write(10, '(F13.5)', advance='no') littler_value(record%z)            ! height
        write(10, '(I7)',    advance='no') 0                                  ! height QC
        write(10, '(F13.5)', advance='no') littler_value(T)                   ! temperature (K)
        write(10, '(I7)',    advance='no') 0                                  ! temperature QC
        write(10, '(F13.5)', advance='no') littler_value(Td)                  ! dewpoint (K)
        write(10, '(I7)',    advance='no') 0                                  ! dewpoint QC
        write(10, '(F13.5)', advance='no') littler_value(record%ws)           ! wind speed (m s^-1)
        write(10, '(I7)',    advance='no') 0                                  ! wind QC
        write(10, '(F13.5)', advance='no') littler_value(record%wd)           ! wind direction (degree)
        write(10, '(I7)',    advance='no') 0                                  ! wind QC
        write(10, '(F13.5)', advance='no') littler_value(real_missing_value_in_littler)         ! wind u component (m s^-1)
        write(10, '(I7)',    advance='no') 0                                                    ! wind u component QC
        write(10, '(F13.5)', advance='no') real_missing_value_in_littler      ! wind v component (m s^-1)
        write(10, '(I7)',    advance='no') 0                                  ! wind v component QC
        write(10, '(F13.5)', advance='no') littler_value(record%rh)           ! relative humidity (%)
        write(10, '(I7)',    advance='no') 0                                  ! relative humidity QC
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
        write(10, '(F13.5)', advance='no') real_missing_value_in_littler      ! thickness (m))
        write(10, '(I7)',    advance='no') 0                                  ! thickness QC
        write(10, *)
      end select
      call record_iterator%next()
      write(10, '(3I7)') 1, 0, 0
      i = i + 1
    end do

    close(10)

  end subroutine amdar_littler_write

end module amdar_littler_mod
