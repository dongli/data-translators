module synop_odb_mod

  use hash_table_mod
  use linked_list_mod
  use synop_mod
  use odbql_wrappers
  use utils_mod

  private

  public synop_odb_write

contains

  subroutine synop_odb_write(file_path, stations, records)

    character(*), intent(inout) :: file_path
    type(hash_table_type), intent(inout) :: stations
    type(linked_list_type), intent(inout) :: records

    ! ODB variables
    type(odbql) odb_db
    type(odbql_stmt) odb_stmt
    character(100) :: odb_unparsed_sql = ''

    integer col
    type(linked_list_iterator_type) record_iterator

    if (file_path == '') file_path = 'synop.odb'

    ! Write ODB file.
    call odbql_open('', odb_db)
    call odbql_prepare_v2(odb_db, 'CREATE TABLE synop AS (' // &
      'platform_id STRING, '               // &
      'lon REAL, '                         // &
      'lat REAL, '                         // &
      'z REAL, '                           // &
      'date STRING, '                      // &
      'time STRING, '                      // &
      'type INTEGER, '                     // &
      'temperature REAL, '                 // &
      'temperature_qc INTEGER, '           // &
      'dewpoint REAL, '                    // &
      'relative_humidity REAL, '           // &
      'relative_humidity_qc INTEGER, '     // &
      'wind_u REAL, '                      // &
      'wind_v REAL, '                      // &
      'wind_qc INTEGER, '                  // &
      'pressure REAL, '                    // &
      'pressure_qc INTEGER '               // &
      ') ON "' // trim(file_path) // '";', -1, odb_stmt, odb_unparsed_sql)
    call odbql_prepare_v2(odb_db, 'INSERT INTO synop (' // &
      'platform_id, '                      // &
      'lon, '                              // &
      'lat, '                              // &
      'z, '                                // &
      'date, '                             // &
      'time, '                             // &
      'type, '                             // &
      'temperature, '                      // &
      'temperature_qc, '                   // &
      'dewpoint, '                         // &
      'relative_humidity, '                // &
      'relative_humidity_qc, '             // &
      'wind_u, '                           // &
      'wind_v, '                           // &
      'wind_qc, '                          // &
      'pressure,'                          // &
      'pressure_qc'                        // &
      ') VALUES (' // odb_values_placeholder(17) // ');', -1, odb_stmt, odb_unparsed_sql)

    record_iterator = linked_list_iterator(records)
    do while (.not. record_iterator%ended())
      select type (record => record_iterator%value)
      type is (synop_record_type)
        col = 0
        col = col + 1; call odbql_bind_text  (odb_stmt, col, record%station%name, len_trim(record%station%name))
        col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%lon))
        col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%lat))
        col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%z))
        col = col + 1; call odbql_bind_text  (odb_stmt, col, trim(record%time%format('%Y%m%d')), 8)
        col = col + 1; call odbql_bind_text  (odb_stmt, col, trim(record%time%format('%H%M%S')), 6)
        col = col + 1; call odbql_bind_int   (odb_stmt, col, record%type)
        col = col + 1; if (.not. is_missing(record%temperature))          call odbql_bind_double(odb_stmt, col, dble(record%temperature))
        col = col + 1; if (.not. is_missing(record%temperature_qc))       call odbql_bind_int   (odb_stmt, col, record%temperature_qc)
        col = col + 1; if (.not. is_missing(record%dewpoint))             call odbql_bind_double(odb_stmt, col, dble(record%dewpoint))
        col = col + 1; if (.not. is_missing(record%relative_humidity))    call odbql_bind_double(odb_stmt, col, dble(record%relative_humidity))
        col = col + 1; if (.not. is_missing(record%relative_humidity_qc)) call odbql_bind_int   (odb_stmt, col, record%relative_humidity_qc)
        col = col + 1; if (.not. is_missing(record%wind_u))               call odbql_bind_double(odb_stmt, col, dble(record%wind_u))
        col = col + 1; if (.not. is_missing(record%wind_v))               call odbql_bind_double(odb_stmt, col, dble(record%wind_v))
        col = col + 1; if (.not. is_missing(record%wind_qc))              call odbql_bind_int   (odb_stmt, col, record%wind_qc)
        col = col + 1; if (.not. is_missing(record%pressure))             call odbql_bind_double(odb_stmt, col, dble(record%pressure))
        col = col + 1; if (.not. is_missing(record%pressure_qc))          call odbql_bind_int   (odb_stmt, col, record%pressure_qc)
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

  end subroutine synop_odb_write

end module synop_odb_mod
