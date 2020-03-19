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
      'platform_id STRING, '        // &
      'platform_type STRING, '      // &
      'source STRING, '             // &
      'lon REAL, '                  // &
      'lat REAL, '                  // &
      'date INTEGER, '              // &
      'time INTEGER, '              // &
      'p REAL, '                    // &
      'p_qc INTEGER, '              // &
      'ta REAL, '                   // &
      'ta_qc INTEGER, '             // &
      'sst REAL, '                  // &
      'sst_qc INTEGER, '            // &
      'td REAL, '                   // &
      'td_qc INTEGER, '             // &
      'rh REAL, '                   // &
      'rh_qc INTEGER, '             // &
      'sh REAL, '                   // &
      'sh_qc INTEGER, '             // &
      'ua REAL, '                   // &
      'ua_qc INTEGER, '             // &
      'va REAL, '                   // &
      'va_qc INTEGER, '             // &
      'hww REAL, '                  // &
      'hww_qc INTEGER, '            // &
      'pww REAL, '                  // &
      'pww_qc INTEGER, '            // &
      'hsw REAL, '                  // &
      'hsw_qc INTEGER, '            // &
      'psw REAL, '                  // &
      'psw_qc INTEGER, '            // &
      'dsw REAL, '                  // &
      'dsw_qc INTEGER, '            // &
      'vis REAL, '                  // &
      'vis_qc INTEGER, '            // &
      'clc REAL, '                  // &
      'clc_qc INTEGER'              // &
      ') ON "' // trim(file_path) // '";', -1, odb_stmt, odb_unparsed_sql)
    call odbql_prepare_v2(odb_db, 'INSERT INTO ship (' // &
      'platform_id, '               // &
      'platform_type, '             // &
      'source, '                    // &
      'lon, '                       // &
      'lat, '                       // &
      'date, '                      // &
      'time, '                      // &
      'p, '                         // &
      'p_qc, '                      // &
      'ta, '                        // &
      'ta_qc, '                     // &
      'sst, '                       // &
      'sst_qc, '                    // &
      'td, '                        // &
      'td_qc, '                     // &
      'rh, '                        // &
      'rh_qc, '                     // &
      'sh, '                        // &
      'sh_qc, '                     // &
      'ua, '                        // &
      'ua_qc, '                     // &
      'va, '                        // &
      'va_qc, '                     // &
      'hww, '                       // &
      'hww_qc, '                    // &
      'pww, '                       // &
      'pww_qc, '                    // &
      'hsw, '                       // &
      'hsw_qc, '                    // &
      'psw, '                       // &
      'psw_qc, '                    // &
      'dsw, '                       // &
      'dsw_qc, '                    // &
      'vis, '                       // &
      'vis_qc, '                    // &
      'clc, '                       // &
      'clc_qc'                      // &
      ') VALUES (' // odb_values_placeholder(36) // ');', -1, odb_stmt, odb_unparsed_sql)

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
        col = col + 1; if (.not. is_missing(record%p     )) call odbql_bind_double(odb_stmt, col, dble(record%p    ))
        col = col + 1; if (.not. is_missing(record%p_qc  )) call odbql_bind_int   (odb_stmt, col,      record%p_qc  )
        col = col + 1; if (.not. is_missing(record%ta    )) call odbql_bind_double(odb_stmt, col, dble(record%ta   ))
        col = col + 1; if (.not. is_missing(record%ta_qc )) call odbql_bind_int   (odb_stmt, col,      record%ta_qc )
        col = col + 1; if (.not. is_missing(record%sst   )) call odbql_bind_double(odb_stmt, col, dble(record%sst  ))
        col = col + 1; if (.not. is_missing(record%sst_qc)) call odbql_bind_int   (odb_stmt, col,      record%sst_qc)
        col = col + 1; if (.not. is_missing(record%td    )) call odbql_bind_double(odb_stmt, col, dble(record%td   ))
        col = col + 1; if (.not. is_missing(record%td_qc )) call odbql_bind_int   (odb_stmt, col,      record%td_qc )
        col = col + 1; if (.not. is_missing(record%rh    )) call odbql_bind_double(odb_stmt, col, dble(record%rh   ))
        col = col + 1; if (.not. is_missing(record%rh_qc )) call odbql_bind_int   (odb_stmt, col,      record%rh_qc )
        col = col + 1; if (.not. is_missing(record%sh    )) call odbql_bind_double(odb_stmt, col, dble(record%sh   ))
        col = col + 1; if (.not. is_missing(record%sh_qc )) call odbql_bind_int   (odb_stmt, col,      record%sh_qc )
        col = col + 1; if (.not. is_missing(record%ua    )) call odbql_bind_double(odb_stmt, col, dble(record%ua   ))
        col = col + 1; if (.not. is_missing(record%ua_qc )) call odbql_bind_int   (odb_stmt, col,      record%ua_qc )
        col = col + 1; if (.not. is_missing(record%va    )) call odbql_bind_double(odb_stmt, col, dble(record%va   ))
        col = col + 1; if (.not. is_missing(record%va_qc )) call odbql_bind_int   (odb_stmt, col,      record%va_qc )
        col = col + 1; if (.not. is_missing(record%hww   )) call odbql_bind_double(odb_stmt, col, dble(record%hww  ))
        col = col + 1; if (.not. is_missing(record%hww_qc)) call odbql_bind_int   (odb_stmt, col,      record%hww_qc)
        col = col + 1; if (.not. is_missing(record%pww   )) call odbql_bind_double(odb_stmt, col, dble(record%pww  ))
        col = col + 1; if (.not. is_missing(record%pww_qc)) call odbql_bind_int   (odb_stmt, col,      record%pww_qc)
        col = col + 1; if (.not. is_missing(record%hsw   )) call odbql_bind_double(odb_stmt, col, dble(record%hsw  ))
        col = col + 1; if (.not. is_missing(record%hsw_qc)) call odbql_bind_int   (odb_stmt, col,      record%hsw_qc)
        col = col + 1; if (.not. is_missing(record%psw   )) call odbql_bind_double(odb_stmt, col, dble(record%psw  ))
        col = col + 1; if (.not. is_missing(record%psw_qc)) call odbql_bind_int   (odb_stmt, col,      record%psw_qc)
        col = col + 1; if (.not. is_missing(record%dsw   )) call odbql_bind_double(odb_stmt, col, dble(record%dsw  ))
        col = col + 1; if (.not. is_missing(record%dsw_qc)) call odbql_bind_int   (odb_stmt, col,      record%dsw_qc)
        col = col + 1; if (.not. is_missing(record%vis   )) call odbql_bind_double(odb_stmt, col, dble(record%vis  ))
        col = col + 1; if (.not. is_missing(record%vis_qc)) call odbql_bind_int   (odb_stmt, col,      record%vis_qc)
        col = col + 1; if (.not. is_missing(record%clc   )) call odbql_bind_double(odb_stmt, col, dble(record%clc  ))
        col = col + 1; if (.not. is_missing(record%clc_qc)) call odbql_bind_int   (odb_stmt, col,      record%clc_qc)
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
