module ship_odb_mod

  use hash_table_mod
  use linked_list_mod
  use ship_mod
  use utils_mod
  use odbql_wrappers

  implicit none

  private

  public ship_odb_write

contains


  subroutine ship_odb_write(file_path, ships, records)

    character(*), intent(inout) :: file_path
    type(hash_table_type), intent(inout) :: ships
    type(linked_list_type), intent(inout) :: records

    ! ODB variables
    type(odbql) odb_db
    type(odbql_stmt) odb_stmt
    character(100) odb_unparsed_sql

    integer col
    type(linked_list_iterator_type) record_iterator

    if (records%size == 0) then
      file_path = ''
      return
    end if

    if (file_path == '') file_path = 'ship.odb'

    ! Write ODB file.
    call odbql_open('', odb_db)
    call odbql_prepare_v2(odb_db, 'CREATE TABLE ship AS (' // &
      'platform_id STRING, '               // &
      'platform_type STRING, '             // &
      'source STRING, '                    // &
      'lon REAL, '                         // &
      'lat REAL, '                         // &
      'date INTEGER, '                     // &
      'time INTEGER, '                     // &
      'pressure REAL, '                    // &
      'pressure_qc INTEGER, '              // &
      'air_temperature REAL, '             // &
      'air_temperature_qc INTEGER, '       // &
      'sea_temperature REAL, '             // &
      'sea_temperature_qc INTEGER, '       // &
      'dewpoint REAL, '                    // &
      'dewpoint_qc INTEGER, '              // &
      'relative_humidity REAL, '           // &
      'relative_humidity_qc INTEGER, '     // &
      'specific_humidity REAL, '           // &
      'specific_humidity_qc INTEGER, '     // &
      'wind_u REAL, '                      // &
      'wind_v REAL, '                      // &
      'wind_qc INTEGER, '                  // &
      'wind_wave_height REAL, '            // &
      'wind_wave_height_qc INTEGER, '      // &
      'wind_wave_period REAL, '            // &
      'wind_wave_period_qc INTEGER, '      // &
      'surge_wave_height REAL, '           // &
      'surge_wave_height_qc INTEGER, '     // &
      'surge_wave_period REAL, '           // &
      'surge_wave_period_qc INTEGER, '     // &
      'surge_wave_direction REAL, '        // &
      'surge_wave_direction_qc INTEGER, '  // &
      'visibility REAL, '                  // &
      'visibility_qc INTEGER, '            // &
      'cloud_cover REAL, '                 // &
      'cloud_cover_qc INTEGER'             // &
      ') ON "' // trim(file_path) // '";', -1, odb_stmt, odb_unparsed_sql)
    call odbql_prepare_v2(odb_db, 'INSERT INTO ship (' // &
      'platform_id, '                      // &
      'platform_type, '                    // &
      'source, '                           // &
      'lon, '                              // &
      'lat, '                              // &
      'date, '                             // &
      'time, '                             // &
      'pressure, '                         // &
      'pressure_qc, '                      // &
      'air_temperature, '                  // &
      'air_temperature_qc, '               // &
      'sea_temperature, '                  // &
      'sea_temperature_qc, '               // &
      'dewpoint, '                         // &
      'dewpoint_qc, '                      // &
      'relative_humidity, '                // &
      'relative_humidity_qc, '             // &
      'specific_humidity, '                // &
      'specific_humidity_qc, '             // &
      'wind_u, '                           // &
      'wind_v, '                           // &
      'wind_qc, '                          // &
      'wind_wave_height, '                 // &
      'wind_wave_height_qc, '              // &
      'wind_wave_period, '                 // &
      'wind_wave_period_qc, '              // &
      'surge_wave_height, '                // &
      'surge_wave_height_qc, '             // &
      'surge_wave_period, '                // &
      'surge_wave_period_qc, '             // &
      'surge_wave_direction, '             // &
      'surge_wave_direction_qc, '          // &
      'visibility_qc, '                    // &
      'cloud_cover, '                      // &
      'cloud_cover_qc'                     // &
      ') VALUES (' // odb_values_placeholder(35) // ');', -1, odb_stmt, odb_unparsed_sql)

    record_iterator = linked_list_iterator(records)
    do while (.not. record_iterator%ended())
      select type (record => record_iterator%value)
      type is (ship_record_type)
        col = 0
        col = col + 1; call odbql_bind_text  (odb_stmt, col, trim(record%ship%name), len_trim(record%ship%name))
        col = col + 1;
        col = col + 1; call odbql_bind_text  (odb_stmt, col, trim(record%source), len_trim(record%source))
        col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%lon))
        col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%lat))
        col = col + 1; call odbql_bind_int   (odb_stmt, col, to_integer(record%time%format('%Y%m%d')))
        col = col + 1; call odbql_bind_int   (odb_stmt, col, to_integer(record%time%format('%H%M%S')))
        col = col + 1; if (.not. is_missing(record%pressure))                call odbql_bind_double(odb_stmt, col, dble(record%pressure))
        col = col + 1; if (.not. is_missing(record%pressure_qc))             call odbql_bind_int   (odb_stmt, col, record%pressure_qc)
        col = col + 1; if (.not. is_missing(record%air_temperature))         call odbql_bind_double(odb_stmt, col, dble(record%air_temperature))
        col = col + 1; if (.not. is_missing(record%air_temperature_qc))      call odbql_bind_int   (odb_stmt, col, record%air_temperature_qc)
        col = col + 1; if (.not. is_missing(record%sea_temperature))         call odbql_bind_double(odb_stmt, col, dble(record%sea_temperature))
        col = col + 1; if (.not. is_missing(record%sea_temperature_qc))      call odbql_bind_int   (odb_stmt, col, record%sea_temperature_qc)
        col = col + 1; if (.not. is_missing(record%dewpoint))                call odbql_bind_double(odb_stmt, col, dble(record%dewpoint))
        col = col + 1; if (.not. is_missing(record%dewpoint_qc))             call odbql_bind_int   (odb_stmt, col, record%dewpoint_qc)
        col = col + 1; if (.not. is_missing(record%relative_humidity))       call odbql_bind_double(odb_stmt, col, dble(record%relative_humidity))
        col = col + 1; if (.not. is_missing(record%relative_humidity_qc))    call odbql_bind_int   (odb_stmt, col, record%relative_humidity_qc)
        col = col + 1; if (.not. is_missing(record%specific_humidity))       call odbql_bind_double(odb_stmt, col, dble(record%specific_humidity))
        col = col + 1; if (.not. is_missing(record%specific_humidity_qc))    call odbql_bind_int   (odb_stmt, col, record%specific_humidity_qc)
        col = col + 1; if (.not. is_missing(record%wind_u))                  call odbql_bind_double(odb_stmt, col, dble(record%wind_u))
        col = col + 1; if (.not. is_missing(record%wind_v))                  call odbql_bind_double(odb_stmt, col, dble(record%wind_v))
        col = col + 1; if (.not. is_missing(record%wind_qc))                 call odbql_bind_int   (odb_stmt, col, record%wind_qc)
        col = col + 1; if (.not. is_missing(record%wind_wave_height))        call odbql_bind_double(odb_stmt, col, dble(record%wind_wave_height))
        col = col + 1; if (.not. is_missing(record%wind_wave_height_qc))     call odbql_bind_int   (odb_stmt, col, record%wind_wave_height_qc)
        col = col + 1; if (.not. is_missing(record%wind_wave_period))        call odbql_bind_double(odb_stmt, col, dble(record%wind_wave_period))
        col = col + 1; if (.not. is_missing(record%wind_wave_period_qc))     call odbql_bind_int   (odb_stmt, col, record%wind_wave_period_qc)
        col = col + 1; if (.not. is_missing(record%surge_wave_height))       call odbql_bind_double(odb_stmt, col, dble(record%surge_wave_height))
        col = col + 1; if (.not. is_missing(record%surge_wave_height_qc))    call odbql_bind_int   (odb_stmt, col, record%surge_wave_height_qc)
        col = col + 1; if (.not. is_missing(record%surge_wave_period))       call odbql_bind_double(odb_stmt, col, dble(record%surge_wave_period))
        col = col + 1; if (.not. is_missing(record%surge_wave_period_qc))    call odbql_bind_int   (odb_stmt, col, record%surge_wave_period_qc)
        col = col + 1; if (.not. is_missing(record%surge_wave_direction))    call odbql_bind_double(odb_stmt, col, dble(record%surge_wave_direction))
        col = col + 1; if (.not. is_missing(record%surge_wave_direction_qc)) call odbql_bind_int   (odb_stmt, col, record%surge_wave_direction_qc)
        col = col + 1; if (.not. is_missing(record%visibility))              call odbql_bind_double(odb_stmt, col, dble(record%visibility))
        col = col + 1; if (.not. is_missing(record%visibility_qc))           call odbql_bind_int   (odb_stmt, col, record%visibility_qc)
        col = col + 1; if (.not. is_missing(record%cloud_cover))             call odbql_bind_double(odb_stmt, col, dble(record%cloud_cover))
        col = col + 1; if (.not. is_missing(record%cloud_cover_qc))          call odbql_bind_int   (odb_stmt, col, record%cloud_cover_qc)
        call odbql_step(odb_stmt)
        call odb_all_bind_null(odb_stmt, col)
      class default
        write(*, *) '[Error]: Unknown record in the list!'
        stop 1
      end select
      call record_iterator%next()
    end do

    call odbql_finalize(odb_stmt)
    call odbql_close(odb_db)
    write(*, *) '[Notice]: ODB file is written.'

  end subroutine ship_odb_write

end module ship_odb_mod
