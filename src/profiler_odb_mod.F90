module profiler_odb_mod

  use hash_table_mod
  use linked_list_mod
  use profiler_mod
  use utils_mod
  use odbql_wrappers

  implicit none

  private

  public profiler_odb_write

contains

  subroutine profiler_odb_write(file_path, stations, records)

    character(*), intent(inout) :: file_path
    type(hash_table_type), intent(inout) :: stations
    type(linked_list_type), intent(inout) :: records

    ! ODB variables
    type(odbql) odb_db
    type(odbql_stmt) odb_stmt
    character(100) odb_unparsed_sql

    integer col, k
    type(linked_list_iterator_type) record_iterator

    if (file_path == '') file_path = 'profiler.odb'

    ! Write ODB file.
    call odbql_open('', odb_db)
    call odbql_prepare_v2(odb_db, 'CREATE TABLE profiler AS (' // &
      'station_name STRING, '                     // &
      'lon REAL, '                                // &
      'lat REAL, '                                // &
      'z REAL, '                                  // &
      'date STRING, '                             // &
      'time STRING, '                             // &
      'pressure REAL, '                           // &
      'height REAL, '                             // &
      'wind_u REAL, '                             // &
      'wind_v REAL'                               // &
      ') ON "' // trim(file_path) // '";', -1, odb_stmt, odb_unparsed_sql)
    call odbql_prepare_v2(odb_db, 'INSERT INTO profiler (' // &
      'station_name, '                            // &
      'lon, '                                     // &
      'lat, '                                     // &
      'z, '                                       // &
      'date, '                                    // &
      'time, '                                    // &
      'pressure, '                                // &
      'height, '                                  // &
      'wind_u, '                                  // &
      'wind_v'                                    // &
      ') VALUES (' // odb_values_placeholder(10) // ');', -1, odb_stmt, odb_unparsed_sql)

    record_iterator = linked_list_iterator(records)
    do while (.not. record_iterator%ended())
      select type (record => record_iterator%value)
      type is (profiler_record_type)
        do k = 1, record%pro%num_level
          col = 0
          col = col + 1; call odbql_bind_text  (odb_stmt, col, record%station%name, len_trim(record%station%name))
          col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%lon))
          col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%lat))
          col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%z))
          col = col + 1; call odbql_bind_text  (odb_stmt, col, trim(record%time%format('%Y%m%d')), 8)
          col = col + 1; call odbql_bind_text  (odb_stmt, col, trim(record%time%format('%H%M%S')), 6)
          col = col + 1; if (.not. is_missing(record%pro%pressure(k))) call odbql_bind_double(odb_stmt, col, dble(record%pro%pressure(k)))
          col = col + 1; if (.not. is_missing(record%pro%height(k)))   call odbql_bind_double(odb_stmt, col, dble(record%pro%height(k)))
          col = col + 1; if (.not. is_missing(record%pro%wind_u(k)))   call odbql_bind_double(odb_stmt, col, dble(record%pro%wind_u(k)))
          col = col + 1; if (.not. is_missing(record%pro%wind_v(k)))   call odbql_bind_double(odb_stmt, col, dble(record%pro%wind_v(k)))
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

  end subroutine profiler_odb_write

end module profiler_odb_mod
