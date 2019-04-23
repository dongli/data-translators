module raob_odb_mod

  use hash_table_mod
  use linked_list_mod
  use raob_mod
  use odbql_wrappers
  use utils_mod

  implicit none

  private

  public raob_odb_write

contains

  subroutine raob_odb_write(file_path, stations, records)

    character(*), intent(inout) :: file_path
    type(hash_table_type), intent(inout) :: stations
    type(linked_list_type), intent(inout) :: records

    ! ODB variables
    type(odbql) odb_db
    type(odbql_stmt) odb_stmt
    character(100) odb_unparsed_sql

    integer col, k
    type(linked_list_iterator_type) record_iterator

    if (file_path == '') file_path = 'raob.odb'

    ! Write ODB file.
    call odbql_open('', odb_db)
    call odbql_prepare_v2(odb_db, 'CREATE TABLE raob AS (' // &
      'platform_id STRING, '                // &
      'lon REAL, '                          // &
      'lat REAL, '                          // &
      'z REAL, '                            // &
      'level_type STRING, '                 // &
      'date STRING, '                       // &
      'time STRING, '                       // &
      'height REAL, '                       // &
      'height_qc INTEGER, '                 // &
      'pressure REAL, '                     // &
      'pressure_qc INTEGER, '               // &
      'temperature REAL, '                  // &
      'temperature_qc INTEGER, '            // &
      'dewpoint REAL, '                     // &
      'specific_humidity REAL, '            // &
      'specific_humidity_qc INTEGER, '      // &
      'wind_u REAL, '                       // &
      'wind_v REAL, '                       // &
      'wind_qc INTEGER'                     // &
      ') ON "' // trim(file_path) // '";', -1, odb_stmt, odb_unparsed_sql)
    call odbql_prepare_v2(odb_db, 'INSERT INTO raob (' // &
      'platform_id, '                       // &
      'lon, '                               // &
      'lat, '                               // &
      'z, '                                 // &
      'level_type, '                        // &
      'date, '                              // &
      'time, '                              // &
      'height, '                            // &
      'height_qc, '                         // &
      'pressure, '                          // &
      'pressure_qc, '                       // &
      'temperature, '                       // &
      'temperature_qc, '                    // &
      'dewpoint, '                          // &
      'specific_humidity, '                 // &
      'specific_humidity_qc, '              // &
      'wind_u, '                            // &
      'wind_v, '                            // &
      'wind_qc'                             // &
      ') VALUES (' // trim(odb_values_placeholder(19)) // ');', -1, odb_stmt, odb_unparsed_sql)

    record_iterator = linked_list_iterator(records)
    do while (.not. record_iterator%ended())
      select type (record => record_iterator%value)
      type is (raob_record_type)
        ! Surface level
        col = 0
        col = col + 1; call odbql_bind_text  (odb_stmt, col, record%station%name, len_trim(record%station%name))
        col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%lon))
        col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%lat))
        col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%z))
        col = col + 1; call odbql_bind_text  (odb_stmt, col, 'sfc', 3)
        col = col + 1; call odbql_bind_text  (odb_stmt, col, trim(record%time%format('%Y%m%d')), 8)
        col = col + 1; call odbql_bind_text  (odb_stmt, col, trim(record%time%format('%H%M%S')), 6)
        col = col + 1;
        col = col + 1;
        col = col + 1; if (.not. is_missing(record%snd_sfc_pressure))             call odbql_bind_double(odb_stmt, col, dble(record%snd_sfc_pressure))
        col = col + 1; if (.not. is_missing(record%snd_sfc_pressure_qc))          call odbql_bind_int   (odb_stmt, col, record%snd_sfc_pressure_qc)
        col = col + 1; if (.not. is_missing(record%snd_sfc_temperature))          call odbql_bind_double(odb_stmt, col, dble(record%snd_sfc_temperature))
        col = col + 1; if (.not. is_missing(record%snd_sfc_temperature_qc))       call odbql_bind_int   (odb_stmt, col, record%snd_sfc_temperature_qc)
        col = col + 1; if (.not. is_missing(record%snd_sfc_dewpoint))             call odbql_bind_double(odb_stmt, col, dble(record%snd_sfc_dewpoint))
        col = col + 1; if (.not. is_missing(record%snd_sfc_specific_humidity))    call odbql_bind_double(odb_stmt, col, dble(record%snd_sfc_specific_humidity))
        col = col + 1; if (.not. is_missing(record%snd_sfc_specific_humidity_qc)) call odbql_bind_double(odb_stmt, col, dble(record%snd_sfc_specific_humidity_qc))
        col = col + 1; if (.not. is_missing(record%snd_sfc_wind_u))               call odbql_bind_double(odb_stmt, col, dble(record%snd_sfc_wind_u))
        col = col + 1; if (.not. is_missing(record%snd_sfc_wind_v))               call odbql_bind_double(odb_stmt, col, dble(record%snd_sfc_wind_v))
        col = col + 1; if (.not. is_missing(record%snd_sfc_wind_qc))              call odbql_bind_double(odb_stmt, col, dble(record%snd_sfc_wind_qc))
        call odbql_step(odb_stmt)
        call odb_all_bind_null(odb_stmt, col)
        ! Mandatory levels
        do k = 1, record%snd_man%num_level
          col = 0
          col = col + 1; call odbql_bind_text  (odb_stmt, col, record%station%name, len_trim(record%station%name))
          col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%lon))
          col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%lat))
          col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%z))
          col = col + 1; call odbql_bind_text  (odb_stmt, col, 'man', 3)
          col = col + 1; call odbql_bind_text  (odb_stmt, col, trim(record%time%format('%Y%m%d')), 8)
          col = col + 1; call odbql_bind_text  (odb_stmt, col, trim(record%time%format('%H%M%S')), 6)
          col = col + 1; if (.not. is_missing(record%snd_man%height(k)))               call odbql_bind_double(odb_stmt, col, dble(record%snd_man%height(k)))
          col = col + 1; if (.not. is_missing(record%snd_man%height_qc(k)))            call odbql_bind_double(odb_stmt, col, dble(record%snd_man%height_qc(k)))
          col = col + 1; if (.not. is_missing(record%snd_man%pressure(k)))             call odbql_bind_double(odb_stmt, col, dble(record%snd_man%pressure(k)))
          col = col + 1; if (.not. is_missing(record%snd_man%pressure_qc(k)))          call odbql_bind_double(odb_stmt, col, dble(record%snd_man%pressure_qc(k)))
          col = col + 1; if (.not. is_missing(record%snd_man%temperature(k)))          call odbql_bind_double(odb_stmt, col, dble(record%snd_man%temperature(k)))
          col = col + 1; if (.not. is_missing(record%snd_man%temperature_qc(k)))       call odbql_bind_double(odb_stmt, col, dble(record%snd_man%temperature_qc(k)))
          col = col + 1; if (.not. is_missing(record%snd_man%dewpoint(k)))             call odbql_bind_double(odb_stmt, col, dble(record%snd_man%dewpoint(k)))
          col = col + 1; if (.not. is_missing(record%snd_man%specific_humidity(k)))    call odbql_bind_double(odb_stmt, col, dble(record%snd_man%specific_humidity(k)))
          col = col + 1; if (.not. is_missing(record%snd_man%specific_humidity_qc(k))) call odbql_bind_double(odb_stmt, col, dble(record%snd_man%specific_humidity_qc(k)))
          col = col + 1; if (.not. is_missing(record%snd_man%wind_u(k)))               call odbql_bind_double(odb_stmt, col, dble(record%snd_man%wind_u(k)))
          col = col + 1; if (.not. is_missing(record%snd_man%wind_v(k)))               call odbql_bind_double(odb_stmt, col, dble(record%snd_man%wind_v(k)))
          col = col + 1; if (.not. is_missing(record%snd_man%wind_qc(k)))              call odbql_bind_double(odb_stmt, col, dble(record%snd_man%wind_qc(k)))
          call odbql_step(odb_stmt)
          call odb_all_bind_null(odb_stmt, col)
        end do
        ! Significant temperature levels
        do k = 1, record%snd_sigt%num_level
          col = 0
          col = col + 1; call odbql_bind_text  (odb_stmt, col, record%station%name, len_trim(record%station%name))
          col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%lon))
          col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%lat))
          col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%z))
          col = col + 1; call odbql_bind_text  (odb_stmt, col, 'sigt', 4)
          col = col + 1; call odbql_bind_text  (odb_stmt, col, trim(record%time%format('%Y%m%d')), 8)
          col = col + 1; call odbql_bind_text  (odb_stmt, col, trim(record%time%format('%H%M%S')), 6)
          col = col + 1; if (.not. is_missing(record%snd_sigt%height(k)))               call odbql_bind_double(odb_stmt, col, dble(record%snd_sigt%height(k)))
          col = col + 1; if (.not. is_missing(record%snd_sigt%height_qc(k)))            call odbql_bind_double(odb_stmt, col, dble(record%snd_sigt%height_qc(k)))
          col = col + 1; if (.not. is_missing(record%snd_sigt%pressure(k)))             call odbql_bind_double(odb_stmt, col, dble(record%snd_sigt%pressure(k)))
          col = col + 1; if (.not. is_missing(record%snd_sigt%pressure_qc(k)))          call odbql_bind_double(odb_stmt, col, dble(record%snd_sigt%pressure_qc(k)))
          col = col + 1; if (.not. is_missing(record%snd_sigt%temperature(k)))          call odbql_bind_double(odb_stmt, col, dble(record%snd_sigt%temperature(k)))
          col = col + 1; if (.not. is_missing(record%snd_sigt%temperature_qc(k)))       call odbql_bind_double(odb_stmt, col, dble(record%snd_sigt%temperature_qc(k)))
          col = col + 1; if (.not. is_missing(record%snd_sigt%dewpoint(k)))             call odbql_bind_double(odb_stmt, col, dble(record%snd_sigt%dewpoint(k)))
          col = col + 1; if (.not. is_missing(record%snd_sigt%specific_humidity(k)))    call odbql_bind_double(odb_stmt, col, dble(record%snd_sigt%specific_humidity(k)))
          col = col + 1; if (.not. is_missing(record%snd_sigt%specific_humidity_qc(k))) call odbql_bind_double(odb_stmt, col, dble(record%snd_sigt%specific_humidity_qc(k)))
          col = col + 1; if (.not. is_missing(record%snd_sigt%wind_u(k)))               call odbql_bind_double(odb_stmt, col, dble(record%snd_sigt%wind_u(k)))
          col = col + 1; if (.not. is_missing(record%snd_sigt%wind_v(k)))               call odbql_bind_double(odb_stmt, col, dble(record%snd_sigt%wind_v(k)))
          col = col + 1; if (.not. is_missing(record%snd_sigt%wind_qc(k)))              call odbql_bind_double(odb_stmt, col, dble(record%snd_sigt%wind_qc(k)))
          call odbql_step(odb_stmt)
          call odb_all_bind_null(odb_stmt, col)
        end do
        ! Significant wind levels
        do k = 1, record%snd_sigw%num_level
          col = 0
          col = col + 1; call odbql_bind_text  (odb_stmt, col, record%station%name, len_trim(record%station%name))
          col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%lon))
          col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%lat))
          col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%z))
          col = col + 1; call odbql_bind_text  (odb_stmt, col, 'sigw', 4)
          col = col + 1; call odbql_bind_text  (odb_stmt, col, trim(record%time%format('%Y%m%d')), 8)
          col = col + 1; call odbql_bind_text  (odb_stmt, col, trim(record%time%format('%H%M%S')), 6)
          col = col + 1; if (.not. is_missing(record%snd_sigw%height(k)))               call odbql_bind_double(odb_stmt, col, dble(record%snd_sigw%height(k)))
          col = col + 1; if (.not. is_missing(record%snd_sigw%height_qc(k)))            call odbql_bind_double(odb_stmt, col, dble(record%snd_sigw%height_qc(k)))
          col = col + 1; if (.not. is_missing(record%snd_sigw%pressure(k)))             call odbql_bind_double(odb_stmt, col, dble(record%snd_sigw%pressure(k)))
          col = col + 1; if (.not. is_missing(record%snd_sigw%pressure_qc(k)))          call odbql_bind_double(odb_stmt, col, dble(record%snd_sigw%pressure_qc(k)))
          col = col + 1; if (.not. is_missing(record%snd_sigw%temperature(k)))          call odbql_bind_double(odb_stmt, col, dble(record%snd_sigw%temperature(k)))
          col = col + 1; if (.not. is_missing(record%snd_sigw%temperature_qc(k)))       call odbql_bind_double(odb_stmt, col, dble(record%snd_sigw%temperature_qc(k)))
          col = col + 1; if (.not. is_missing(record%snd_sigw%dewpoint(k)))             call odbql_bind_double(odb_stmt, col, dble(record%snd_sigw%dewpoint(k)))
          col = col + 1; if (.not. is_missing(record%snd_sigw%specific_humidity(k)))    call odbql_bind_double(odb_stmt, col, dble(record%snd_sigw%specific_humidity(k)))
          col = col + 1; if (.not. is_missing(record%snd_sigw%specific_humidity_qc(k))) call odbql_bind_double(odb_stmt, col, dble(record%snd_sigw%specific_humidity_qc(k)))
          col = col + 1; if (.not. is_missing(record%snd_sigw%wind_u(k)))               call odbql_bind_double(odb_stmt, col, dble(record%snd_sigw%wind_u(k)))
          col = col + 1; if (.not. is_missing(record%snd_sigw%wind_v(k)))               call odbql_bind_double(odb_stmt, col, dble(record%snd_sigw%wind_v(k)))
          col = col + 1; if (.not. is_missing(record%snd_sigw%wind_qc(k)))              call odbql_bind_double(odb_stmt, col, dble(record%snd_sigw%wind_qc(k)))
          call odbql_step(odb_stmt)
          call odb_all_bind_null(odb_stmt, col)
        end do
        ! Tropopause levels
        do k = 1, record%snd_trop%num_level
          col = 0
          col = col + 1; call odbql_bind_text  (odb_stmt, col, record%station%name, len_trim(record%station%name))
          col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%lon))
          col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%lat))
          col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%z))
          col = col + 1; call odbql_bind_text  (odb_stmt, col, 'trop', 4)
          col = col + 1; call odbql_bind_text  (odb_stmt, col, trim(record%time%format('%Y%m%d')), 8)
          col = col + 1; call odbql_bind_text  (odb_stmt, col, trim(record%time%format('%H%M%S')), 6)
          col = col + 1; if (.not. is_missing(record%snd_trop%height(k)))               call odbql_bind_double(odb_stmt, col, dble(record%snd_trop%height(k)))
          col = col + 1; if (.not. is_missing(record%snd_trop%height_qc(k)))            call odbql_bind_double(odb_stmt, col, dble(record%snd_trop%height_qc(k)))
          col = col + 1; if (.not. is_missing(record%snd_trop%pressure(k)))             call odbql_bind_double(odb_stmt, col, dble(record%snd_trop%pressure(k)))
          col = col + 1; if (.not. is_missing(record%snd_trop%pressure_qc(k)))          call odbql_bind_double(odb_stmt, col, dble(record%snd_trop%pressure_qc(k)))
          col = col + 1; if (.not. is_missing(record%snd_trop%temperature(k)))          call odbql_bind_double(odb_stmt, col, dble(record%snd_trop%temperature(k)))
          col = col + 1; if (.not. is_missing(record%snd_trop%temperature_qc(k)))       call odbql_bind_double(odb_stmt, col, dble(record%snd_trop%temperature_qc(k)))
          col = col + 1; if (.not. is_missing(record%snd_trop%dewpoint(k)))             call odbql_bind_double(odb_stmt, col, dble(record%snd_trop%dewpoint(k)))
          col = col + 1; if (.not. is_missing(record%snd_trop%specific_humidity(k)))    call odbql_bind_double(odb_stmt, col, dble(record%snd_trop%specific_humidity(k)))
          col = col + 1; if (.not. is_missing(record%snd_trop%specific_humidity_qc(k))) call odbql_bind_double(odb_stmt, col, dble(record%snd_trop%specific_humidity_qc(k)))
          col = col + 1; if (.not. is_missing(record%snd_trop%wind_u(k)))               call odbql_bind_double(odb_stmt, col, dble(record%snd_trop%wind_u(k)))
          col = col + 1; if (.not. is_missing(record%snd_trop%wind_v(k)))               call odbql_bind_double(odb_stmt, col, dble(record%snd_trop%wind_v(k)))
          col = col + 1; if (.not. is_missing(record%snd_trop%wind_qc(k)))              call odbql_bind_double(odb_stmt, col, dble(record%snd_trop%wind_qc(k)))
          call odbql_step(odb_stmt)
          call odb_all_bind_null(odb_stmt, col)
        end do
      class default
        write(*, *) '[Error]: Unknown record in the list!'
        stop 1
      end select
      call record_iterator%next()
    end do

    call odbql_finalize(odb_stmt)
    call odbql_close(odb_db)
    write(*, *) '[Notice]: ODB file is written.'

  end subroutine raob_odb_write

end module raob_odb_mod
