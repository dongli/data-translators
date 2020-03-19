module raob_odb_mod

  use hash_table_mod
  use linked_list_mod
  use raob_mod
  use odbql_wrappers
  use utils_mod

  implicit none

  private

  public raob_odb_write

contains

  subroutine raob_odb_write(file_path, stations, records)

    character(*), intent(inout) :: file_path
    type(hash_table_type), intent(inout) :: stations
    type(linked_list_type), intent(inout) :: records

    ! ODB variables
    type(odbql) odb_db
    type(odbql_stmt) odb_stmt
    character(100) odb_unparsed_sql

    integer col, k
    type(linked_list_iterator_type) record_iterator

    if (file_path == '') file_path = 'raob.odb'

    ! Write ODB file.
    call odbql_open('', odb_db)
    call odbql_prepare_v2(odb_db, 'CREATE TABLE raob AS (' // &
      'platform_id STRING, '                // &
      'platform_type STRING, '              // &
      'source STRING, '                     // &
      'lon REAL, '                          // &
      'lat REAL, '                          // &
      'z REAL, '                            // &
      'level_type STRING, '                 // &
      'date INTEGER, '                      // &
      'time INTEGER, '                      // &
      'p REAL, '                            // &
      'p_qc INTEGER, '                      // &
      'p_cr REAL, '                         // &
      'h REAL, '                            // &
      'h_qc INTEGER, '                      // &
      'h_cr REAL, '                         // &
      'ta REAL, '                           // &
      'ta_qc INTEGER, '                     // &
      'ta_cr REAL, '                        // &
      'td REAL, '                           // &
      'sh REAL, '                           // &
      'sh_qc INTEGER, '                     // &
      'sh_cr REAL, '                        // &
      'ua REAL, '                           // &
      'ua_qc INTEGER,'                      // &
      'ua_cr REAL, '                        // &
      'va REAL, '                           // &
      'va_qc INTEGER,'                      // &
      'va_cr REAL'                          // &
      ') ON "' // trim(file_path) // '";', -1, odb_stmt, odb_unparsed_sql)
    call odbql_prepare_v2(odb_db, 'INSERT INTO raob (' // &
      'platform_id, '                       // &
      'platform_type, '                     // &
      'source, '                            // &
      'lon, '                               // &
      'lat, '                               // &
      'z, '                                 // &
      'level_type, '                        // &
      'date, '                              // &
      'time, '                              // &
      'p, '                                 // &
      'p_qc, '                              // &
      'p_cr, '                              // &
      'h, '                                 // &
      'h_qc, '                              // &
      'h_cr, '                              // &
      'ta, '                                // &
      'ta_qc, '                             // &
      'ta_cr, '                             // &
      'td, '                                // &
      'sh, '                                // &
      'sh_qc, '                             // &
      'sh_cr, '                             // &
      'ua, '                                // &
      'ua_qc, '                             // &
      'ua_cr, '                             // &
      'va, '                                // &
      'va_qc, '                             // &
      'va_cr'                               // &
      ') VALUES (' // trim(odb_values_placeholder(28)) // ');', -1, odb_stmt, odb_unparsed_sql)

    record_iterator = linked_list_iterator(records)
    do while (.not. record_iterator%ended())
      select type (record => record_iterator%value)
      type is (raob_record_type)
        ! Surface level
        col = 0
        col = col + 1; call odbql_bind_text  (odb_stmt, col, record%station%name, len_trim(record%station%name))
        col = col + 1; call odbql_bind_text  (odb_stmt, col, record%platform_type, len_trim(record%platform_type))
        col = col + 1;
        col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%lon))
        col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%lat))
        col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%z))
        col = col + 1; call odbql_bind_text  (odb_stmt, col, 'sfc', 3)
        col = col + 1; call odbql_bind_int   (odb_stmt, col, to_integer(record%time%format('%Y%m%d')))
        col = col + 1; call odbql_bind_int   (odb_stmt, col, to_integer(record%time%format('%H%M%S')))
        col = col + 1; if (.not. is_missing(record%ps   ))  call odbql_bind_double(odb_stmt, col, dble(record%ps   ))
        col = col + 1; if (.not. is_missing(record%ps_qc))  call odbql_bind_int   (odb_stmt, col,      record%ps_qc)
        col = col + 1; if (.not. is_missing(record%ps_cr))  call odbql_bind_double(odb_stmt, col, dble(record%ps_cr))
        col = col + 1;
        col = col + 1;
        col = col + 1;
        col = col + 1; if (.not. is_missing(record%tas   )) call odbql_bind_double(odb_stmt, col, dble(record%tas   ))
        col = col + 1; if (.not. is_missing(record%tas_qc)) call odbql_bind_int   (odb_stmt, col,      record%tas_qc)
        col = col + 1; if (.not. is_missing(record%tas_cr)) call odbql_bind_double(odb_stmt, col, dble(record%tas_cr))
        col = col + 1; if (.not. is_missing(record%tds   )) call odbql_bind_double(odb_stmt, col, dble(record%tds   ))
        col = col + 1; if (.not. is_missing(record%shs   )) call odbql_bind_double(odb_stmt, col, dble(record%shs   ))
        col = col + 1; if (.not. is_missing(record%shs_qc)) call odbql_bind_int   (odb_stmt, col,      record%shs_qc)
        col = col + 1; if (.not. is_missing(record%shs_cr)) call odbql_bind_double(odb_stmt, col, dble(record%shs_cr))
        col = col + 1; if (.not. is_missing(record%uas   )) call odbql_bind_double(odb_stmt, col, dble(record%uas   ))
        col = col + 1; if (.not. is_missing(record%uas_qc)) call odbql_bind_int   (odb_stmt, col,      record%uas_qc)
        col = col + 1; if (.not. is_missing(record%uas_cr)) call odbql_bind_double(odb_stmt, col, dble(record%uas_cr))
        col = col + 1; if (.not. is_missing(record%vas   )) call odbql_bind_double(odb_stmt, col, dble(record%vas   ))
        col = col + 1; if (.not. is_missing(record%vas_qc)) call odbql_bind_int   (odb_stmt, col,      record%vas_qc)
        col = col + 1; if (.not. is_missing(record%vas_cr)) call odbql_bind_double(odb_stmt, col, dble(record%vas_cr))
        call odbql_step(odb_stmt)
        call odb_all_bind_null(odb_stmt, col)
        ! Mandatory levels
        do k = 1, record%man%num_level
          col = 0
          col = col + 1; call odbql_bind_text  (odb_stmt, col, record%station%name, len_trim(record%station%name))
          col = col + 1; call odbql_bind_text  (odb_stmt, col, record%platform_type, len_trim(record%platform_type))
          col = col + 1;
          col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%lon))
          col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%lat))
          col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%z))
          col = col + 1; call odbql_bind_text  (odb_stmt, col, 'man', 3)
          col = col + 1; call odbql_bind_int   (odb_stmt, col, to_integer(record%time%format('%Y%m%d')))
          col = col + 1; call odbql_bind_int   (odb_stmt, col, to_integer(record%time%format('%H%M%S')))
          col = col + 1; if (.not. is_missing(record%man%p    (k))) call odbql_bind_double(odb_stmt, col, dble(record%man%p    (k)))
          col = col + 1; if (.not. is_missing(record%man%p_qc (k))) call odbql_bind_int   (odb_stmt, col,      record%man%p_qc (k))
          col = col + 1; if (.not. is_missing(record%man%p_cr (k))) call odbql_bind_double(odb_stmt, col, dble(record%man%p_cr (k)))
          col = col + 1; if (.not. is_missing(record%man%h    (k))) call odbql_bind_double(odb_stmt, col, dble(record%man%h    (k)))
          col = col + 1; if (.not. is_missing(record%man%h_qc (k))) call odbql_bind_int   (odb_stmt, col,      record%man%h_qc (k))
          col = col + 1; if (.not. is_missing(record%man%h_cr (k))) call odbql_bind_double(odb_stmt, col, dble(record%man%h_cr (k)))
          col = col + 1; if (.not. is_missing(record%man%ta   (k))) call odbql_bind_double(odb_stmt, col, dble(record%man%ta   (k)))
          col = col + 1; if (.not. is_missing(record%man%ta_qc(k))) call odbql_bind_int   (odb_stmt, col,      record%man%ta_qc(k))
          col = col + 1; if (.not. is_missing(record%man%ta_cr(k))) call odbql_bind_double(odb_stmt, col, dble(record%man%ta_cr(k)))
          col = col + 1; if (.not. is_missing(record%man%td   (k))) call odbql_bind_double(odb_stmt, col, dble(record%man%td   (k)))
          col = col + 1; if (.not. is_missing(record%man%sh   (k))) call odbql_bind_double(odb_stmt, col, dble(record%man%sh   (k)))
          col = col + 1; if (.not. is_missing(record%man%sh_qc(k))) call odbql_bind_int   (odb_stmt, col,      record%man%sh_qc(k))
          col = col + 1; if (.not. is_missing(record%man%sh_cr(k))) call odbql_bind_double(odb_stmt, col, dble(record%man%sh_cr(k)))
          col = col + 1; if (.not. is_missing(record%man%ua   (k))) call odbql_bind_double(odb_stmt, col, dble(record%man%ua   (k)))
          col = col + 1; if (.not. is_missing(record%man%ua_qc(k))) call odbql_bind_int   (odb_stmt, col,      record%man%ua_qc(k))
          col = col + 1; if (.not. is_missing(record%man%ua_cr(k))) call odbql_bind_double(odb_stmt, col, dble(record%man%ua_cr(k)))
          col = col + 1; if (.not. is_missing(record%man%va   (k))) call odbql_bind_double(odb_stmt, col, dble(record%man%va   (k)))
          col = col + 1; if (.not. is_missing(record%man%va_qc(k))) call odbql_bind_int   (odb_stmt, col,      record%man%va_qc(k))
          col = col + 1; if (.not. is_missing(record%man%va_cr(k))) call odbql_bind_double(odb_stmt, col, dble(record%man%va_cr(k)))
          call odbql_step(odb_stmt)
          call odb_all_bind_null(odb_stmt, col)
        end do
        ! Significant ta levels
        do k = 1, record%sigt%num_level
          col = 0
          col = col + 1; call odbql_bind_text  (odb_stmt, col, record%station%name, len_trim(record%station%name))
          col = col + 1; call odbql_bind_text  (odb_stmt, col, record%platform_type, len_trim(record%platform_type))
          col = col + 1;
          col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%lon))
          col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%lat))
          col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%z))
          col = col + 1; call odbql_bind_text  (odb_stmt, col, 'sigt', 4)
          col = col + 1; call odbql_bind_int   (odb_stmt, col, to_integer(record%time%format('%Y%m%d')))
          col = col + 1; call odbql_bind_int   (odb_stmt, col, to_integer(record%time%format('%H%M%S')))
          col = col + 1; if (.not. is_missing(record%sigt%p    (k))) call odbql_bind_double(odb_stmt, col, dble(record%sigt%p    (k)))
          col = col + 1; if (.not. is_missing(record%sigt%p_qc (k))) call odbql_bind_int   (odb_stmt, col,      record%sigt%p_qc (k))
          col = col + 1; if (.not. is_missing(record%sigt%p_cr (k))) call odbql_bind_double(odb_stmt, col, dble(record%sigt%p_cr (k)))
          col = col + 1; if (.not. is_missing(record%sigt%h    (k))) call odbql_bind_double(odb_stmt, col, dble(record%sigt%h    (k)))
          col = col + 1; if (.not. is_missing(record%sigt%h_qc (k))) call odbql_bind_int   (odb_stmt, col,      record%sigt%h_qc (k))
          col = col + 1; if (.not. is_missing(record%sigt%h_cr (k))) call odbql_bind_double(odb_stmt, col, dble(record%sigt%h_cr (k)))
          col = col + 1; if (.not. is_missing(record%sigt%ta   (k))) call odbql_bind_double(odb_stmt, col, dble(record%sigt%ta   (k)))
          col = col + 1; if (.not. is_missing(record%sigt%ta_qc(k))) call odbql_bind_int   (odb_stmt, col,      record%sigt%ta_qc(k))
          col = col + 1; if (.not. is_missing(record%sigt%ta_cr(k))) call odbql_bind_double(odb_stmt, col, dble(record%sigt%ta_cr(k)))
          col = col + 1; if (.not. is_missing(record%sigt%td   (k))) call odbql_bind_double(odb_stmt, col, dble(record%sigt%td   (k)))
          col = col + 1; if (.not. is_missing(record%sigt%sh   (k))) call odbql_bind_double(odb_stmt, col, dble(record%sigt%sh   (k)))
          col = col + 1; if (.not. is_missing(record%sigt%sh_qc(k))) call odbql_bind_int   (odb_stmt, col,      record%sigt%sh_qc(k))
          col = col + 1; if (.not. is_missing(record%sigt%sh_cr(k))) call odbql_bind_double(odb_stmt, col, dble(record%sigt%sh_cr(k)))
          col = col + 1; if (.not. is_missing(record%sigt%ua   (k))) call odbql_bind_double(odb_stmt, col, dble(record%sigt%ua   (k)))
          col = col + 1; if (.not. is_missing(record%sigt%ua_qc(k))) call odbql_bind_int   (odb_stmt, col,      record%sigt%ua_qc(k))
          col = col + 1; if (.not. is_missing(record%sigt%ua_cr(k))) call odbql_bind_double(odb_stmt, col, dble(record%sigt%ua_cr(k)))
          col = col + 1; if (.not. is_missing(record%sigt%va   (k))) call odbql_bind_double(odb_stmt, col, dble(record%sigt%va   (k)))
          col = col + 1; if (.not. is_missing(record%sigt%va_qc(k))) call odbql_bind_int   (odb_stmt, col,      record%sigt%va_qc(k))
          col = col + 1; if (.not. is_missing(record%sigt%va_cr(k))) call odbql_bind_double(odb_stmt, col, dble(record%sigt%va_cr(k)))
          call odbql_step(odb_stmt)
          call odb_all_bind_null(odb_stmt, col)
        end do
        ! Significant wind levels
        do k = 1, record%sigw%num_level
          col = 0
          col = col + 1; call odbql_bind_text  (odb_stmt, col, record%station%name, len_trim(record%station%name))
          col = col + 1; call odbql_bind_text  (odb_stmt, col, record%platform_type, len_trim(record%platform_type))
          col = col + 1;
          col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%lon))
          col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%lat))
          col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%z))
          col = col + 1; call odbql_bind_text  (odb_stmt, col, 'sigw', 4)
          col = col + 1; call odbql_bind_int   (odb_stmt, col, to_integer(record%time%format('%Y%m%d')))
          col = col + 1; call odbql_bind_int   (odb_stmt, col, to_integer(record%time%format('%H%M%S')))
          col = col + 1; if (.not. is_missing(record%sigw%p    (k))) call odbql_bind_double(odb_stmt, col, dble(record%sigw%p    (k)))
          col = col + 1; if (.not. is_missing(record%sigw%p_qc (k))) call odbql_bind_int   (odb_stmt, col,      record%sigw%p_qc (k))
          col = col + 1; if (.not. is_missing(record%sigw%p_cr (k))) call odbql_bind_double(odb_stmt, col, dble(record%sigw%p_cr (k)))
          col = col + 1; if (.not. is_missing(record%sigw%h    (k))) call odbql_bind_double(odb_stmt, col, dble(record%sigw%h    (k)))
          col = col + 1; if (.not. is_missing(record%sigw%h_qc (k))) call odbql_bind_int   (odb_stmt, col,      record%sigw%h_qc (k))
          col = col + 1; if (.not. is_missing(record%sigw%h_cr (k))) call odbql_bind_double(odb_stmt, col, dble(record%sigw%h_cr (k)))
          col = col + 1; if (.not. is_missing(record%sigw%ta   (k))) call odbql_bind_double(odb_stmt, col, dble(record%sigw%ta   (k)))
          col = col + 1; if (.not. is_missing(record%sigw%ta_qc(k))) call odbql_bind_int   (odb_stmt, col,      record%sigw%ta_qc(k))
          col = col + 1; if (.not. is_missing(record%sigw%ta_cr(k))) call odbql_bind_double(odb_stmt, col, dble(record%sigw%ta_cr(k)))
          col = col + 1; if (.not. is_missing(record%sigw%td   (k))) call odbql_bind_double(odb_stmt, col, dble(record%sigw%td   (k)))
          col = col + 1; if (.not. is_missing(record%sigw%sh   (k))) call odbql_bind_double(odb_stmt, col, dble(record%sigw%sh   (k)))
          col = col + 1; if (.not. is_missing(record%sigw%sh_qc(k))) call odbql_bind_int   (odb_stmt, col,      record%sigw%sh_qc(k))
          col = col + 1; if (.not. is_missing(record%sigw%sh_cr(k))) call odbql_bind_double(odb_stmt, col, dble(record%sigw%sh_cr(k)))
          col = col + 1; if (.not. is_missing(record%sigw%ua   (k))) call odbql_bind_double(odb_stmt, col, dble(record%sigw%ua   (k)))
          col = col + 1; if (.not. is_missing(record%sigw%ua_qc(k))) call odbql_bind_int   (odb_stmt, col,      record%sigw%ua_qc(k))
          col = col + 1; if (.not. is_missing(record%sigw%ua_cr(k))) call odbql_bind_double(odb_stmt, col, dble(record%sigw%ua_cr(k)))
          col = col + 1; if (.not. is_missing(record%sigw%va   (k))) call odbql_bind_double(odb_stmt, col, dble(record%sigw%va   (k)))
          col = col + 1; if (.not. is_missing(record%sigw%va_qc(k))) call odbql_bind_int   (odb_stmt, col,      record%sigw%va_qc(k))
          col = col + 1; if (.not. is_missing(record%sigw%va_cr(k))) call odbql_bind_double(odb_stmt, col, dble(record%sigw%va_cr(k)))
          call odbql_step(odb_stmt)
          call odb_all_bind_null(odb_stmt, col)
        end do
        ! Tropopause levels
        do k = 1, record%trop%num_level
          col = 0
          col = col + 1; call odbql_bind_text  (odb_stmt, col, record%station%name, len_trim(record%station%name))
          col = col + 1; call odbql_bind_text  (odb_stmt, col, record%platform_type, len_trim(record%platform_type))
          col = col + 1;
          col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%lon))
          col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%lat))
          col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%z))
          col = col + 1; call odbql_bind_text  (odb_stmt, col, 'trop', 4)
          col = col + 1; call odbql_bind_int   (odb_stmt, col, to_integer(record%time%format('%Y%m%d')))
          col = col + 1; call odbql_bind_int   (odb_stmt, col, to_integer(record%time%format('%H%M%S')))
          col = col + 1; if (.not. is_missing(record%trop%p    (k))) call odbql_bind_double(odb_stmt, col, dble(record%trop%p    (k)))
          col = col + 1; if (.not. is_missing(record%trop%p_qc (k))) call odbql_bind_int   (odb_stmt, col,      record%trop%p_qc (k))
          col = col + 1; if (.not. is_missing(record%trop%p_cr (k))) call odbql_bind_double(odb_stmt, col, dble(record%trop%p_cr (k)))
          col = col + 1; if (.not. is_missing(record%trop%h    (k))) call odbql_bind_double(odb_stmt, col, dble(record%trop%h    (k)))
          col = col + 1; if (.not. is_missing(record%trop%h_qc (k))) call odbql_bind_int   (odb_stmt, col,      record%trop%h_qc (k))
          col = col + 1; if (.not. is_missing(record%trop%h_cr (k))) call odbql_bind_double(odb_stmt, col, dble(record%trop%h_cr (k)))
          col = col + 1; if (.not. is_missing(record%trop%ta   (k))) call odbql_bind_double(odb_stmt, col, dble(record%trop%ta   (k)))
          col = col + 1; if (.not. is_missing(record%trop%ta_qc(k))) call odbql_bind_int   (odb_stmt, col,      record%trop%ta_qc(k))
          col = col + 1; if (.not. is_missing(record%trop%ta_cr(k))) call odbql_bind_double(odb_stmt, col, dble(record%trop%ta_cr(k)))
          col = col + 1; if (.not. is_missing(record%trop%td   (k))) call odbql_bind_double(odb_stmt, col, dble(record%trop%td   (k)))
          col = col + 1; if (.not. is_missing(record%trop%sh   (k))) call odbql_bind_double(odb_stmt, col, dble(record%trop%sh   (k)))
          col = col + 1; if (.not. is_missing(record%trop%sh_qc(k))) call odbql_bind_int   (odb_stmt, col,      record%trop%sh_qc(k))
          col = col + 1; if (.not. is_missing(record%trop%sh_cr(k))) call odbql_bind_double(odb_stmt, col, dble(record%trop%sh_cr(k)))
          col = col + 1; if (.not. is_missing(record%trop%ua   (k))) call odbql_bind_double(odb_stmt, col, dble(record%trop%ua   (k)))
          col = col + 1; if (.not. is_missing(record%trop%ua_qc(k))) call odbql_bind_int   (odb_stmt, col,      record%trop%ua_qc(k))
          col = col + 1; if (.not. is_missing(record%trop%ua_cr(k))) call odbql_bind_double(odb_stmt, col, dble(record%trop%ua_cr(k)))
          col = col + 1; if (.not. is_missing(record%trop%va   (k))) call odbql_bind_double(odb_stmt, col, dble(record%trop%va   (k)))
          col = col + 1; if (.not. is_missing(record%trop%va_qc(k))) call odbql_bind_int   (odb_stmt, col,      record%trop%va_qc(k))
          col = col + 1; if (.not. is_missing(record%trop%va_cr(k))) call odbql_bind_double(odb_stmt, col, dble(record%trop%va_cr(k)))
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

  end subroutine raob_odb_write

end module raob_odb_mod
