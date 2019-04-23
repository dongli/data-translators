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
      'lon REAL, '                         // &
      'lat REAL, '                         // &
      'date STRING, '                      // &
      'time STRING, '                      // &
      'pressure REAL, '                    // &
      'air_temperature REAL, '             // &
      'sea_temperature REAL, '             // &
      'dewpoint REAL, '                    // &
      'relative_humidity REAL, '           // &
      'specific_humidity REAL, '           // &
      'wind_u REAL, '                      // &
      'wind_v REAL, '                      // &
      'cloud REAL'                         // &
      ') ON "' // trim(file_path) // '";', -1, odb_stmt, odb_unparsed_sql)
    call odbql_prepare_v2(odb_db, 'INSERT INTO ship (' // &
      'platform_id, '                      // &
      'lon, '                              // &
      'lat, '                              // &
      'date, '                             // &
      'time, '                             // &
      'pressure, '                         // &
      'air_temperature, '                  // &
      'sea_temperature, '                  // &
      'dewpoint, '                         // &
      'relative_humidity, '                // &
      'specific_humidity, '                // &
      'wind_u, '                           // &
      'wind_v, '                           // &
      'cloud'                              // &
      ') VALUES (' // odb_values_placeholder(14) // ');', -1, odb_stmt, odb_unparsed_sql)

    record_iterator = linked_list_iterator(records)
    do while (.not. record_iterator%ended())
      select type (record => record_iterator%value)
      type is (ship_record_type)
        col = 0
        col = col + 1; call odbql_bind_text  (odb_stmt, col, trim(record%ship%name), len_trim(record%ship%name))
        col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%lon))
        col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%lat))
        col = col + 1; call odbql_bind_text  (odb_stmt, col, trim(record%time%format('%Y%m%d')), 8)
        col = col + 1; call odbql_bind_text  (odb_stmt, col, trim(record%time%format('%H%M%S')), 6)
        col = col + 1; if (.not. is_missing(record%pressure))          call odbql_bind_double(odb_stmt, col, dble(record%pressure))
        col = col + 1; if (.not. is_missing(record%air_temperature))   call odbql_bind_double(odb_stmt, col, dble(record%air_temperature))
        col = col + 1; if (.not. is_missing(record%sea_temperature))   call odbql_bind_double(odb_stmt, col, dble(record%sea_temperature))
        col = col + 1; if (.not. is_missing(record%dewpoint))          call odbql_bind_double(odb_stmt, col, dble(record%dewpoint))
        col = col + 1; if (.not. is_missing(record%relative_humidity)) call odbql_bind_double(odb_stmt, col, dble(record%relative_humidity))
        col = col + 1; if (.not. is_missing(record%specific_humidity)) call odbql_bind_double(odb_stmt, col, dble(record%specific_humidity))
        col = col + 1; if (.not. is_missing(record%wind_u))            call odbql_bind_double(odb_stmt, col, dble(record%wind_u))
        col = col + 1; if (.not. is_missing(record%wind_v))            call odbql_bind_double(odb_stmt, col, dble(record%wind_v))
        col = col + 1; if (.not. is_missing(record%cloud_cover))       call odbql_bind_double(odb_stmt, col, dble(record%cloud_cover))
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
