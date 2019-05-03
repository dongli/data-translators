module amdar_odb_mod

  use hash_table_mod
  use linked_list_mod
  use amdar_mod
  use odbql_wrappers
  use utils_mod

  implicit none

  private

  public amdar_odb_write

contains


  subroutine amdar_odb_write(file_path, flights, records)

    character(*), intent(inout) :: file_path
    type(hash_table_type), intent(inout) :: flights
    type(linked_list_type), intent(inout) :: records

    ! ODB variables
    type(odbql) odb_db
    type(odbql_stmt) odb_stmt
    character(100) odb_unparsed_sql

    integer col
    type(linked_list_iterator_type) record_iterator

    if (file_path == '') file_path = 'amdar.odb'

    ! Write ODB file.
    call odbql_open('', odb_db)
    call odbql_prepare_v2(odb_db, 'CREATE TABLE amdar AS (' // &
      'platform_id STRING, '                   // &
      'platform_type STRING, '                 // &
      'flight_number STRING, '                 // &
      'source STRING, '                        // &
      'lon REAL, '                             // &
      'lat REAL, '                             // &
      'date INTEGER, '                         // &
      'time INTEGER, '                         // &
      'pressure REAL, '                        // &
      'pressure_qc INTEGER, '                  // &
      'height REAL, '                          // &
      'height_qc INTEGER, '                    // &
      'temperature REAL, '                     // &
      'temperature_qc INTEGER, '               // &
      'specific_humidity REAL, '               // &
      'specific_humidity_qc INTEGER, '         // &
      'wind_u REAL, '                          // &
      'wind_v REAL, '                          // &
      'wind_qc INTEGER, '                      // &
      'turbulence_index INTEGER'               // &
      ') ON "' //trim(file_path) // '";', -1, odb_stmt, odb_unparsed_sql)
    call odbql_prepare_v2(odb_db, 'INSERT INTO amdar (' // &
      'platform_id, '                          // &
      'platform_type, '                        // &
      'flight_number, '                        // &
      'source, '                               // &
      'lon, '                                  // &
      'lat, '                                  // &
      'date, '                                 // &
      'time, '                                 // &
      'pressure, '                             // &
      'pressure_qc, '                          // &
      'height, '                               // &
      'height_qc, '                            // &
      'temperature, '                          // &
      'temperature_qc, '                       // &
      'specific_humidity, '                    // &
      'specific_humidity_qc, '                 // &
      'wind_u, '                               // &
      'wind_v, '                               // &
      'wind_qc, '                              // &
      'turbulence_index'                       // &
      ') VALUES (' // trim(odb_values_placeholder(20)) // ');', -1, odb_stmt, odb_unparsed_sql)

    record_iterator = linked_list_iterator(records)
    do while (.not. record_iterator%ended())
      select type (record => record_iterator%value)
      type is (amdar_record_type)
        col = 0
        col = col + 1; call odbql_bind_text  (odb_stmt, col, record%flight%name, len_trim(record%flight%name))
        col = col + 1;
        col = col + 1; call odbql_bind_text  (odb_stmt, col, record%flight%number, len_trim(record%flight%number))
        col = col + 1;
        col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%lon))
        col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%lat))
        col = col + 1; call odbql_bind_int   (odb_stmt, col, to_integer(record%time%format('%Y%m%d')))
        col = col + 1; call odbql_bind_int   (odb_stmt, col, to_integer(record%time%format('%H%M%S')))
        col = col + 1; if (.not. is_missing(record%pressure))             call odbql_bind_double(odb_stmt, col, dble(record%pressure))
        col = col + 1; if (.not. is_missing(record%pressure_qc))          call odbql_bind_double(odb_stmt, col, dble(record%pressure_qc))
        col = col + 1; if (.not. is_missing(record%height))               call odbql_bind_double(odb_stmt, col, dble(record%height))
        col = col + 1; if (.not. is_missing(record%height_qc))            call odbql_bind_double(odb_stmt, col, dble(record%height_qc))
        col = col + 1; if (.not. is_missing(record%temperature))          call odbql_bind_double(odb_stmt, col, dble(record%temperature))
        col = col + 1; if (.not. is_missing(record%temperature_qc))       call odbql_bind_double(odb_stmt, col, dble(record%temperature_qc))
        col = col + 1; if (.not. is_missing(record%specific_humidity))    call odbql_bind_double(odb_stmt, col, dble(record%specific_humidity))
        col = col + 1; if (.not. is_missing(record%specific_humidity_qc)) call odbql_bind_double(odb_stmt, col, dble(record%specific_humidity_qc))
        col = col + 1; if (.not. is_missing(record%wind_u))               call odbql_bind_double(odb_stmt, col, dble(record%wind_u))
        col = col + 1; if (.not. is_missing(record%wind_v))               call odbql_bind_double(odb_stmt, col, dble(record%wind_v))
        col = col + 1; if (.not. is_missing(record%wind_qc))              call odbql_bind_double(odb_stmt, col, dble(record%wind_qc))
        col = col + 1; if (.not. is_missing(record%turbulence_index))     call odbql_bind_double(odb_stmt, col, dble(record%turbulence_index))
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

  end subroutine amdar_odb_write

end module amdar_odb_mod
