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
      'p REAL, '                                  // &
      'p_qc INTEGER, '                            // &
      'p_cr REAL, '                               // &
      'h REAL, '                             // &
      'h_qc INTEGER, '                            // &
      'h_cr REAL, '                               // &
      'ua REAL, '                                 // &
      'ua_qc INTEGER, '                           // &
      'ua_cr REAL, '                              // &
      'va REAL, '                                 // &
      'va_qc INTEGER, '                           // &
      'va_cr REAL'                                // &
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
      'p, '                                       // &
      'p_qc, '                                    // &
      'p_cr, '                                    // &
      'h, '                                       // &
      'h_qc, '                                    // &
      'h_cr, '                                    // &
      'ua, '                                      // &
      'ua_qc, '                                   // &
      'ua_cr, '                                   // &
      'va, '                                      // &
      'va_qc, '                                   // &
      'va_cr'                                     // &
      ') VALUES (' // odb_values_placeholder(20) // ');', -1, odb_stmt, odb_unparsed_sql)

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
          col = col + 1; if (.not. is_missing(record%pro%p    (k))) call odbql_bind_double(odb_stmt, col, dble(record%pro%p   (k)))
          col = col + 1; if (.not. is_missing(record%pro%p_qc (k))) call odbql_bind_int   (odb_stmt, col,      record%pro%p_qc(k))
          col = col + 1; if (.not. is_missing(record%pro%p_cr (k))) call odbql_bind_double(odb_stmt, col, dble(record%pro%p_cr(k)))
          col = col + 1; if (.not. is_missing(record%pro%h    (k))) call odbql_bind_double(odb_stmt, col, dble(record%pro%h   (k)))
          col = col + 1; if (.not. is_missing(record%pro%h_qc (k))) call odbql_bind_int   (odb_stmt, col,      record%pro%h_qc(k))
          col = col + 1; if (.not. is_missing(record%pro%h_cr (k))) call odbql_bind_double(odb_stmt, col, dble(record%pro%h_cr(k)))
          col = col + 1; if (.not. is_missing(record%pro%ua   (k))) call odbql_bind_double(odb_stmt, col, dble(record%pro%ua  (k)))
          col = col + 1; if (.not. is_missing(record%pro%ua_qc(k))) call odbql_bind_int   (odb_stmt, col,      record%pro%ua_qc(k))
          col = col + 1; if (.not. is_missing(record%pro%ua_cr(k))) call odbql_bind_double(odb_stmt, col, dble(record%pro%ua_cr(k)))
          col = col + 1; if (.not. is_missing(record%pro%va   (k))) call odbql_bind_double(odb_stmt, col, dble(record%pro%va(k)))
          col = col + 1; if (.not. is_missing(record%pro%va_qc(k))) call odbql_bind_int   (odb_stmt, col,      record%pro%va_qc(k))
          col = col + 1; if (.not. is_missing(record%pro%va_cr(k))) call odbql_bind_double(odb_stmt, col, dble(record%pro%va_cr(k)))
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
