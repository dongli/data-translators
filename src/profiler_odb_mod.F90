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
      'platform_id STRING, '                      // &
      'platform_type STRING, '                    // &
      'source STRING, '                           // &
      'lon REAL, '                                // &
      'lat REAL, '                                // &
      'z REAL, '                                  // &
      'date INTEGER, '                            // &
      'time INTEGER, '                            // &
      'pressure REAL, '                           // &
      'pressure_qc INTEGER, '                     // &
      'pressure_correct REAL, '                   // &
      'height REAL, '                             // &
      'height_qc INTEGER, '                       // &
      'height_correct REAL, '                     // &
      'wind_u REAL, '                             // &
      'wind_u_correct REAL, '                     // &
      'wind_v REAL, '                             // &
      'wind_v_correct REAL, '                     // &
      'wind_qc INTEGER'                           // &
      ') ON "' // trim(file_path) // '";', -1, odb_stmt, odb_unparsed_sql)
    call odbql_prepare_v2(odb_db, 'INSERT INTO profiler (' // &
      'platform_id, '                             // &
      'platform_type, '                           // &
      'source, '                                  // &
      'lon, '                                     // &
      'lat, '                                     // &
      'z, '                                       // &
      'date, '                                    // &
      'time, '                                    // &
      'pressure, '                                // &
      'pressure_qc, '                             // &
      'pressure_correct, '                        // &
      'height, '                                  // &
      'height_qc, '                               // &
      'height_correct, '                          // &
      'wind_u, '                                  // &
      'wind_u_correct, '                          // &
      'wind_v, '                                  // &
      'wind_v_correct, '                          // &
      'wind_qc'                                   // &
      ') VALUES (' // odb_values_placeholder(19) // ');', -1, odb_stmt, odb_unparsed_sql)

    record_iterator = linked_list_iterator(records)
    do while (.not. record_iterator%ended())
      select type (record => record_iterator%value)
      type is (profiler_record_type)
        do k = 1, record%pro%num_level
          col = 0
          col = col + 1; call odbql_bind_text  (odb_stmt, col, record%station%name, len_trim(record%station%name))
          col = col + 1;
          col = col + 1;
          col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%lon))
          col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%lat))
          col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%z))
          col = col + 1; call odbql_bind_int   (odb_stmt, col, to_integer(record%time%format('%Y%m%d')))
          col = col + 1; call odbql_bind_int   (odb_stmt, col, to_integer(record%time%format('%H%M%S')))
          col = col + 1; if (.not. is_missing(record%pro%pressure(k)))         call odbql_bind_double(odb_stmt, col, dble(record%pro%pressure(k)))
          col = col + 1; if (.not. is_missing(record%pro%pressure_qc(k)))      call odbql_bind_int   (odb_stmt, col, record%pro%pressure_qc(k))
          col = col + 1; if (.not. is_missing(record%pro%pressure_correct(k))) call odbql_bind_double(odb_stmt, col, dble(record%pro%pressure_correct(k)))
          col = col + 1; if (.not. is_missing(record%pro%height(k)))           call odbql_bind_double(odb_stmt, col, dble(record%pro%height(k)))
          col = col + 1; if (.not. is_missing(record%pro%height_qc(k)))        call odbql_bind_int   (odb_stmt, col, record%pro%height_qc(k))
          col = col + 1; if (.not. is_missing(record%pro%height_correct(k)))   call odbql_bind_double(odb_stmt, col, dble(record%pro%height_correct(k)))
          col = col + 1; if (.not. is_missing(record%pro%wind_u(k)))           call odbql_bind_double(odb_stmt, col, dble(record%pro%wind_u(k)))
          col = col + 1; if (.not. is_missing(record%pro%wind_u_correct(k)))   call odbql_bind_double(odb_stmt, col, dble(record%pro%wind_u_correct(k)))
          col = col + 1; if (.not. is_missing(record%pro%wind_v(k)))           call odbql_bind_double(odb_stmt, col, dble(record%pro%wind_v(k)))
          col = col + 1; if (.not. is_missing(record%pro%wind_v_correct(k)))   call odbql_bind_double(odb_stmt, col, dble(record%pro%wind_v_correct(k)))
          col = col + 1; if (.not. is_missing(record%pro%wind_qc(k)))          call odbql_bind_int   (odb_stmt, col, record%pro%wind_qc(k))
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
