module profiler_littler_mod

  use datetime
  use profiler_mod
  use hash_table_mod
  use linked_list_mod
  use params_mod
  use utils_mod
  use string_mod

  implicit none

  private

  public profiler_littler_write

contains

  subroutine profiler_littler_write(file_path)

    character(*), intent(inout) :: file_path

    type(linked_list_iterator_type) record_iterator
    type(hash_table_iterator_type) level_iterator
    integer i

    if (file_path == '') file_path = 'profiler.littler'

    open(10, file=file_path, form='formatted')

    i = 1
    record_iterator = linked_list_iterator(records)
    do while (.not. record_iterator%ended())
      select type (record => record_iterator%value)
      type is (profiler_read_record_type)
        ! Header
        write(10, '(F20.5)', advance='no') record%station%lat                                 ! latitude
        write(10, '(F20.5)', advance='no') record%station%lon                                 ! longitude
        write(10, '(A40)',   advance='no') pad_string(record%station%name, 40)                ! id
        write(10, '(A40)',   advance='no') pad_string(record%station%name, 40)                ! name
        write(10, '(A40)',   advance='no') pad_string('FM-132 PROFLR', 40)                    ! platform
        write(10, '(A40)',   advance='no') pad_string('N/A', 40)                              ! source
        write(10, '(F20.5)', advance='no') record%station%z                                   ! elevation
        write(10, '(I10)',   advance='no') 1                                                  ! num_vld_fld
        write(10, '(I10)',   advance='no') 0                                                  ! num_error
        write(10, '(I10)',   advance='no') 0                                                  ! num_warning
        write(10, '(I10)',   advance='no') i                                                  ! seq_num
        write(10, '(I10)',   advance='no') 0                                                  ! num_dups
        write(10, '(L10)',   advance='no') .true.                                             ! is_sound
        write(10, '(L10)',   advance='no') .false.                                            ! bogus
        write(10, '(L10)',   advance='no') .false.                                            ! discard
        write(10, '(I10)',   advance='no') int(record%time%timestamp())                       ! obs_time
        write(10, '(I10)',   advance='no') record%time%days_in_year()                         ! julian_day
        write(10, '(A20)',   advance='no') pad_string(record%time%format('%Y%m%d%H%M%S'), 20) ! date_char
        write(10, '(F13.5)', advance='no') real_missing_value                                 ! slp
        write(10, '(I7)',    advance='no') 0                                                  ! slp QC
        write(10, '(F13.5)', advance='no') real_missing_value                                 ! ref_pres
        write(10, '(I7)',    advance='no') 0                                                  ! ref_pres QC
        write(10, '(F13.5)', advance='no') real_missing_value                                 ! ground_t
        write(10, '(I7)',    advance='no') 0                                                  ! ground_t QC
        write(10, '(F13.5)', advance='no') real_missing_value                                 ! SST
        write(10, '(I7)',    advance='no') 0                                                  ! SST QC
        write(10, '(F13.5)', advance='no') real_missing_value                                 ! psfc
        write(10, '(I7)',    advance='no') 0                                                  ! psfc QC
        write(10, '(F13.5)', advance='no') real_missing_value                                 ! precip
        write(10, '(I7)',    advance='no') 0                                                  ! precip QC
        write(10, '(F13.5)', advance='no') real_missing_value                                 ! t_max
        write(10, '(I7)',    advance='no') 0                                                  ! t_max QC
        write(10, '(F13.5)', advance='no') real_missing_value                                 ! t_min
        write(10, '(I7)',    advance='no') 0                                                  ! t_min QC
        write(10, '(F13.5)', advance='no') real_missing_value                                 ! t_max night
        write(10, '(I7)',    advance='no') 0                                                  ! t_max night QC
        write(10, '(F13.5)', advance='no') real_missing_value                                 ! p_tend03
        write(10, '(I7)',    advance='no') 0                                                  ! p_tend03 QC
        write(10, '(F13.5)', advance='no') real_missing_value                                 ! p_tend24
        write(10, '(I7)',    advance='no') 0                                                  ! p_tend24 QC
        write(10, '(F13.5)', advance='no') real_missing_value                                 ! clound_cover
        write(10, '(I7)',    advance='no') 0                                                  ! clound_cover QC
        write(10, '(F13.5)', advance='no') real_missing_value                                 ! ceiling
        write(10, '(I7)',    advance='no') 0                                                  ! celing QC
        write(10, *)
        level_iterator = hash_table_iterator(record%pro_pressure)
        do while (.not. level_iterator%ended())
          ! pressure (Pa)
          select type (value => level_iterator%value)
          type is (real)
            write(10, '(F13.5)', advance='no') value
          class default
            write(10, '(F13.5)', advance='no') real_missing_value_in_littler
          end select
          write(10, '(I7)', advance='no') 0
          ! height (m)
          select type (value => record%pro_height%value(level_iterator%key))
          type is (real)
            write(10, '(F13.5)', advance='no') value
          class default
            write(10, '(F13.5)', advance='no') real_missing_value_in_littler
          end select
          write(10, '(I7)', advance='no') 0
          ! temperature (K)
          write(10, '(F13.5)', advance='no') real_missing_value_in_littler
          write(10, '(I7)', advance='no') 0
          ! dewpoint (K)
          write(10, '(F13.5)', advance='no') real_missing_value_in_littler
          write(10, '(I7)', advance='no') 0
          ! wind speed (m/s)
          select type (value => record%pro_wind_speed%value(level_iterator%key))
          type is (real)
            write(10, '(F13.5)', advance='no') value
          class default
            write(10, '(F13.5)', advance='no') real_missing_value_in_littler
          end select
          write(10, '(I7)', advance='no') 0
          ! wind direction (degree)
          select type (value => record%pro_wind_direction%value(level_iterator%key))
          type is (real)
            write(10, '(F13.5)', advance='no') value
          class default
            write(10, '(F13.5)', advance='no') real_missing_value_in_littler
          end select
          write(10, '(I7)', advance='no') 0
          ! wind u component (m/s)
          select type (value => record%pro_wind_u%value(level_iterator%key))
          type is (real)
            write(10, '(F13.5)', advance='no') value
          class default
            write(10, '(F13.5)', advance='no') real_missing_value_in_littler
          end select
          write(10, '(I7)', advance='no') 0
          ! wind v component
          select type (value => record%pro_wind_v%value(level_iterator%key))
          type is (real)
            write(10, '(F13.5)', advance='no') value
          class default
            write(10, '(F13.5)', advance='no') real_missing_value_in_littler
          end select
          write(10, '(I7)', advance='no') 0
          ! relative humidity (%)
          write(10, '(F13.5)', advance='no') real_missing_value_in_littler
          write(10, '(I7)', advance='no') 0
          ! thickness (m)
          write(10, '(F13.5)', advance='no') real_missing_value_in_littler
          write(10, '(I7)',    advance='no') 0
          write(10, *)
          call level_iterator%next()
        end do
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
        write(10, '(F13.5)', advance='no') real_missing_value_in_littler      ! thickness (m)
        write(10, '(I7)',    advance='no') 0                                  ! thickness QC
        write(10, *)
      end select
      call record_iterator%next()
      i = i + 1
    end do
    write(10, '(3I7)') i, 0, 0

    close(10)

  end subroutine profiler_littler_write

end module profiler_littler_mod
