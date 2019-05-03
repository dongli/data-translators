module metar_odb_mod

  use hash_table_mod
  use linked_list_mod
  use metar_mod
  use odbql_wrappers
  use utils_mod

  private

  public metar_odb_write

contains

  subroutine metar_odb_write(file_path, stations, records)

    character(*), intent(inout) :: file_path
    type(hash_table_type), intent(inout) :: stations
    type(linked_list_type), intent(inout) :: records

    ! ODB variables
    type(odbql) odb_db
    type(odbql_stmt) odb_stmt
    character(100) :: odb_unparsed_sql = ''

    integer col
    type(linked_list_iterator_type) record_iterator

    if (file_path == '') file_path = 'metar.odb'

    ! Write ODB file.
    call odbql_open('', odb_db)
    call odbql_prepare_v2(odb_db, 'CREATE TABLE metar AS (' // &
      'platform_id STRING, '               // &
      'platform_type STRING, '             // &
      'source STRING, '                    // &
      'lon REAL, '                         // &
      'lat REAL, '                         // &
      'z REAL, '                           // &
      'date INTEGER, '                     // &
      'time INTEGER, '                     // &
      'temperature REAL, '                 // &
      'temperature_qc INTEGER, '           // &
      'temperature_correct REAL, '         // &
      'dewpoint REAL, '                    // &
      'relative_humidity REAL, '           // &
      'relative_humidity_qc INTEGER, '     // &
      'wind_u REAL, '                      // &
      'wind_v REAL, '                      // &
      'wind_qc INTEGER, '                  // &
      'wind_u_correct REAL, '              // &
      'wind_v_correct REAL, '              // &
      'pressure REAL, '                    // &
      'pressure_qc INTEGER, '              // &
      'pressure_correct REAL'              // &
      ') ON "' // trim(file_path) // '";', -1, odb_stmt, odb_unparsed_sql)
    call odbql_prepare_v2(odb_db, 'INSERT INTO metar (' // &
      'platform_id, '                      // &
      'platform_type, '                    // &
      'source, '                           // &
      'lon, '                              // &
      'lat, '                              // &
      'z, '                                // &
      'date, '                             // &
      'time, '                             // &
      'temperature, '                      // &
      'temperature_qc, '                   // &
      'temperature_correct, '              // &
      'dewpoint, '                         // &
      'relative_humidity, '                // &
      'relative_humidity_qc, '             // &
      'wind_u, '                           // &
      'wind_v, '                           // &
      'wind_qc, '                          // &
      'wind_u_correct, '                   // &
      'wind_v_correct, '                   // &
      'pressure, '                         // &
      'pressure_qc, '                      // &
      'pressure_correct'                   // &
      ') VALUES (' // odb_values_placeholder(22) // ');', -1, odb_stmt, odb_unparsed_sql)

    record_iterator = linked_list_iterator(records)
    do while (.not. record_iterator%ended())
      select type (record => record_iterator%value)
      type is (metar_record_type)
        col = 0
        col = col + 1; call odbql_bind_text  (odb_stmt, col, record%station%name, len_trim(record%station%name))
        col = col + 1;
        col = col + 1;
        col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%lon))
        col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%lat))
        col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%z))
        col = col + 1; call odbql_bind_int   (odb_stmt, col, to_integer(record%time%format('%Y%m%d')))
        col = col + 1; call odbql_bind_int   (odb_stmt, col, to_integer(record%time%format('%H%M%S')))
        col = col + 1; if (.not. is_missing(record%temperature))          call odbql_bind_double(odb_stmt, col, dble(record%temperature))
        col = col + 1; if (.not. is_missing(record%temperature_qc))       call odbql_bind_int   (odb_stmt, col, record%temperature_qc)
        col = col + 1; if (.not. is_missing(record%temperature_correct))  call odbql_bind_double(odb_stmt, col, dble(record%temperature_correct))
        col = col + 1; if (.not. is_missing(record%dewpoint))             call odbql_bind_double(odb_stmt, col, dble(record%dewpoint))
        col = col + 1; if (.not. is_missing(record%relative_humidity))    call odbql_bind_double(odb_stmt, col, dble(record%relative_humidity))
        col = col + 1; if (.not. is_missing(record%relative_humidity_qc)) call odbql_bind_int   (odb_stmt, col, record%relative_humidity_qc)
        col = col + 1; if (.not. is_missing(record%wind_u))               call odbql_bind_double(odb_stmt, col, dble(record%wind_u))
        col = col + 1; if (.not. is_missing(record%wind_v))               call odbql_bind_double(odb_stmt, col, dble(record%wind_v))
        col = col + 1; if (.not. is_missing(record%wind_qc))              call odbql_bind_int   (odb_stmt, col, record%wind_qc)
        col = col + 1; if (.not. is_missing(record%wind_u_correct))       call odbql_bind_double(odb_stmt, col, dble(record%wind_u_correct))
        col = col + 1; if (.not. is_missing(record%wind_v_correct))       call odbql_bind_double(odb_stmt, col, dble(record%wind_v_correct))
        col = col + 1; if (.not. is_missing(record%pressure))             call odbql_bind_double(odb_stmt, col, dble(record%pressure))
        col = col + 1; if (.not. is_missing(record%pressure_qc))          call odbql_bind_int   (odb_stmt, col, record%pressure_qc)
        col = col + 1; if (.not. is_missing(record%pressure_correct))     call odbql_bind_double(odb_stmt, col, dble(record%pressure_correct))
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

  end subroutine metar_odb_write

end module metar_odb_mod
