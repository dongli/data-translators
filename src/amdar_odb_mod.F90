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
      'platform_id STRING, '   // &
      'platform_type STRING, ' // &
      'source STRING, '        // &
      'flight_number STRING, ' // &
      'lon REAL, '             // &
      'lat REAL, '             // &
      'date INTEGER, '         // &
      'time INTEGER, '         // &
      'p REAL, '               // &
      'p_qc INTEGER, '         // &
      'p_cr REAL, '            // &
      'h REAL, '               // &
      'h_qc INTEGER, '         // &
      'h_cr REAL, '            // &
      'ta REAL, '              // &
      'ta_qc INTEGER, '        // &
      'ta_cr REAL, '           // &
      'sh REAL, '              // &
      'sh_qc INTEGER, '        // &
      'sh_cr REAL, '           // &
      'ua REAL, '              // &
      'ua_qc INTEGER, '        // &
      'ua_cr REAL, '           // &
      'va REAL, '              // &
      'va_qc INTEGER, '        // &
      'va_cr REAL, '           // &
      'trb INTEGER'            // &
      ') ON "' //trim(file_path) // '";', -1, odb_stmt, odb_unparsed_sql)
    call odbql_prepare_v2(odb_db, 'INSERT INTO amdar (' // &
      'platform_id, '          // &
      'platform_type, '        // &
      'source, '               // &
      'flight_number, '        // &
      'lon, '                  // &
      'lat, '                  // &
      'date, '                 // &
      'time, '                 // &
      'p, '                    // &
      'p_qc, '                 // &
      'p_cr, '                 // &
      'h, '                    // &
      'h_qc, '                 // &
      'h_cr, '                 // &
      'ta, '                   // &
      'ta_qc, '                // &
      'ta_cr, '                // &
      'sh, '                   // &
      'sh_qc, '                // &
      'sh_cr, '                // &
      'ua, '                   // &
      'ua_qc, '                // &
      'ua_cr, '                // &
      'va, '                   // &
      'va_qc, '                // &
      'va_cr, '                // &
      'trb'                    // &
      ') VALUES (' // trim(odb_values_placeholder(27)) // ');', -1, odb_stmt, odb_unparsed_sql)

    record_iterator = linked_list_iterator(records)
    do while (.not. record_iterator%ended())
      select type (record => record_iterator%value)
      type is (amdar_record_type)
        col = 0
        col = col + 1; call odbql_bind_text  (odb_stmt, col, record%flight%name, len_trim(record%flight%name))
        col = col + 1; call odbql_bind_text  (odb_stmt, col, record%platform_type, len_trim(record%platform_type))
        col = col + 1;
        col = col + 1; call odbql_bind_text  (odb_stmt, col, record%flight%number, len_trim(record%flight%number))
        col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%lon))
        col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%lat))
        col = col + 1; call odbql_bind_int   (odb_stmt, col, to_integer(record%time%format('%Y%m%d')))
        col = col + 1; call odbql_bind_int   (odb_stmt, col, to_integer(record%time%format('%H%M%S')))
        col = col + 1; if (.not. is_missing(record%p    )) call odbql_bind_double(odb_stmt, col, dble(record%p    ))
        col = col + 1; if (.not. is_missing(record%p_qc )) call odbql_bind_int   (odb_stmt, col,      record%p_qc  )
        col = col + 1; if (.not. is_missing(record%p_cr )) call odbql_bind_double(odb_stmt, col, dble(record%p_cr ))
        col = col + 1; if (.not. is_missing(record%h    )) call odbql_bind_double(odb_stmt, col, dble(record%h    ))
        col = col + 1; if (.not. is_missing(record%h_qc )) call odbql_bind_int   (odb_stmt, col,      record%h_qc  )
        col = col + 1; if (.not. is_missing(record%h_cr )) call odbql_bind_double(odb_stmt, col, dble(record%h_cr ))
        col = col + 1; if (.not. is_missing(record%ta   )) call odbql_bind_double(odb_stmt, col, dble(record%ta   ))
        col = col + 1; if (.not. is_missing(record%ta_qc)) call odbql_bind_int   (odb_stmt, col,      record%ta_qc )
        col = col + 1; if (.not. is_missing(record%ta_cr)) call odbql_bind_double(odb_stmt, col, dble(record%ta_cr))
        col = col + 1; if (.not. is_missing(record%sh   )) call odbql_bind_double(odb_stmt, col, dble(record%sh   ))
        col = col + 1; if (.not. is_missing(record%sh_qc)) call odbql_bind_int   (odb_stmt, col,      record%sh_qc )
        col = col + 1; if (.not. is_missing(record%sh_cr)) call odbql_bind_double(odb_stmt, col, dble(record%sh_cr))
        col = col + 1; if (.not. is_missing(record%ua   )) call odbql_bind_double(odb_stmt, col, dble(record%ua   ))
        col = col + 1; if (.not. is_missing(record%ua_qc)) call odbql_bind_int   (odb_stmt, col,      record%ua_qc )
        col = col + 1; if (.not. is_missing(record%ua_cr)) call odbql_bind_double(odb_stmt, col, dble(record%ua_cr))
        col = col + 1; if (.not. is_missing(record%va   )) call odbql_bind_double(odb_stmt, col, dble(record%va   ))
        col = col + 1; if (.not. is_missing(record%va_qc)) call odbql_bind_int   (odb_stmt, col,      record%va_qc )
        col = col + 1; if (.not. is_missing(record%va_cr)) call odbql_bind_double(odb_stmt, col, dble(record%va_cr))
        col = col + 1; if (.not. is_missing(record%trb  )) call odbql_bind_double(odb_stmt, col, dble(record%trb  ))
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
