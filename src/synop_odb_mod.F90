module synop_odb_mod

  use odbql_wrappers
  use container
  use flogger
  use data_translators_utils_mod
  use synop_mod

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
      'platform_id STRING, '   // &
      'platform_type STRING, ' // &
      'source STRING, '        // &
      'lon REAL, '             // &
      'lat REAL, '             // &
      'z REAL, '               // &
      'date INTEGER, '         // &
      'time INTEGER, '         // &
      'ta REAL, '              // &
      'ta_qc INTEGER, '        // &
      'ta_cr REAL, '           // &
      'p REAL, '               // &
      'p_qc INTEGER, '         // &
      'p_cr REAL, '            // &
      'td REAL, '              // &
      'rh REAL, '              // &
      'rh_qc INTEGER, '        // &
      'ua REAL, '              // &
      'ua_qc INTEGER, '        // &
      'ua_cr REAL, '           // &
      'va REAL, '              // &
      'va_qc INTEGER, '        // &
      'va_cr REAL'             // &
      ') ON "' // trim(file_path) // '";', -1, odb_stmt, odb_unparsed_sql)
    call odbql_prepare_v2(odb_db, 'INSERT INTO synop (' // &
      'platform_id, '          // &
      'platform_type, '        // &
      'source, '               // &
      'lon, '                  // &
      'lat, '                  // &
      'z, '                    // &
      'date, '                 // &
      'time, '                 // &
      'ta, '                   // &
      'ta_qc, '                // &
      'ta_cr, '                // &
      'p, '                    // &
      'p_qc, '                 // &
      'p_cr, '                 // &
      'td, '                   // &
      'rh, '                   // &
      'rh_qc, '                // &
      'ua, '                   // &
      'ua_qc, '                // &
      'ua_cr, '                // &
      'va, '                   // &
      'va_qc, '                // &
      'va_cr'                  // &
      ') VALUES (' // odb_values_placeholder(23) // ');', -1, odb_stmt, odb_unparsed_sql)

    record_iterator = linked_list_iterator(records)
    do while (.not. record_iterator%ended())
      select type (record => record_iterator%value)
      type is (synop_record_type)
        col = 0
        col = col + 1; call odbql_bind_text  (odb_stmt, col, record%station%name, len_trim(record%station%name))
        col = col + 1;
        col = col + 1;
        col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%lon))
        col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%lat))
        col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%z))
        col = col + 1; call odbql_bind_int   (odb_stmt, col, to_int(record%time%format('%Y%m%d')))
        col = col + 1; call odbql_bind_int   (odb_stmt, col, to_int(record%time%format('%H%M%S')))
        col = col + 1; if (.not. is_missing(record%ta))    call odbql_bind_double(odb_stmt, col, dble(record%ta   ))
        col = col + 1; if (.not. is_missing(record%ta_qc)) call odbql_bind_int   (odb_stmt, col,      record%ta_qc )
        col = col + 1; if (.not. is_missing(record%ta_cr)) call odbql_bind_double(odb_stmt, col, dble(record%ta_cr))
        col = col + 1; if (.not. is_missing(record%p))     call odbql_bind_double(odb_stmt, col, dble(record%p    ))
        col = col + 1; if (.not. is_missing(record%p_qc))  call odbql_bind_int   (odb_stmt, col,      record%p_qc  )
        col = col + 1; if (.not. is_missing(record%p_cr))  call odbql_bind_double(odb_stmt, col, dble(record%p_cr ))
        col = col + 1; if (.not. is_missing(record%td))    call odbql_bind_double(odb_stmt, col, dble(record%td   ))
        col = col + 1; if (.not. is_missing(record%rh))    call odbql_bind_double(odb_stmt, col, dble(record%rh   ))
        col = col + 1; if (.not. is_missing(record%rh_qc)) call odbql_bind_int   (odb_stmt, col,      record%rh_qc )
        col = col + 1; if (.not. is_missing(record%ua))    call odbql_bind_double(odb_stmt, col, dble(record%ua   ))
        col = col + 1; if (.not. is_missing(record%ua_qc)) call odbql_bind_int   (odb_stmt, col,      record%ua_qc )
        col = col + 1; if (.not. is_missing(record%ua_cr)) call odbql_bind_double(odb_stmt, col, dble(record%ua_cr))
        col = col + 1; if (.not. is_missing(record%va))    call odbql_bind_double(odb_stmt, col, dble(record%va   ))
        col = col + 1; if (.not. is_missing(record%va_qc)) call odbql_bind_int   (odb_stmt, col,      record%va_qc )
        col = col + 1; if (.not. is_missing(record%va_cr)) call odbql_bind_double(odb_stmt, col, dble(record%va_cr))
        call odbql_step(odb_stmt)
        call odb_all_bind_null(odb_stmt, col)
      class default
        call log_error('Unknown record in the list!')
      end select
      call record_iterator%next()
    end do

    call odbql_finalize(odb_stmt)
    call odbql_close(odb_db)

  end subroutine synop_odb_write

end module synop_odb_mod
