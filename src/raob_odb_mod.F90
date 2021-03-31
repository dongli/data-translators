module raob_odb_mod

  use odbql_wrappers
  use container
  use flogger
  use raob_mod
  use data_translators_utils_mod

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
        col = col + 1; call odbql_bind_int   (odb_stmt, col, to_int(record%time%format('%Y%m%d')))
        col = col + 1; call odbql_bind_int   (odb_stmt, col, to_int(record%time%format('%H%M%S')))
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
        call write_profile(odb_stmt, record, record%man , 'man ')
        call write_profile(odb_stmt, record, record%sigt, 'sigt')
        call write_profile(odb_stmt, record, record%sigw, 'sigw')
        call write_profile(odb_stmt, record, record%trop, 'trop')
      class default
        call log_error('Unknown record in the list!')
      end select
      call record_iterator%next()
    end do

    call odbql_finalize(odb_stmt)
    call odbql_close(odb_db)

  end subroutine raob_odb_write

  subroutine write_profile(odb_stmt, record, profile, level_type)

    type(odbql_stmt), intent(inout) :: odb_stmt
    type(raob_record_type), intent(in) :: record
    type(raob_profile_type), intent(in) :: profile
    character(*), intent(in) :: level_type

    integer k, col

    do k = 1, profile%num_level
      col = 0
      col = col + 1; call odbql_bind_text  (odb_stmt, col, record%station%name, len_trim(record%station%name))
      col = col + 1; call odbql_bind_text  (odb_stmt, col, record%platform_type, len_trim(record%platform_type))
      col = col + 1;
      col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%lon))
      col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%lat))
      col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%z))
      col = col + 1; call odbql_bind_text  (odb_stmt, col, level_type, 4)
      col = col + 1; call odbql_bind_int   (odb_stmt, col, to_int(record%time%format('%Y%m%d')))
      col = col + 1; call odbql_bind_int   (odb_stmt, col, to_int(record%time%format('%H%M%S')))
      col = col + 1; if (.not. is_missing(profile%p    (k))) call odbql_bind_double(odb_stmt, col, dble(profile%p    (k)))
      col = col + 1; if (.not. is_missing(profile%p_qc (k))) call odbql_bind_int   (odb_stmt, col,      profile%p_qc (k))
      col = col + 1; if (.not. is_missing(profile%p_cr (k))) call odbql_bind_double(odb_stmt, col, dble(profile%p_cr (k)))
      col = col + 1; if (.not. is_missing(profile%h    (k))) call odbql_bind_double(odb_stmt, col, dble(profile%h    (k)))
      col = col + 1; if (.not. is_missing(profile%h_qc (k))) call odbql_bind_int   (odb_stmt, col,      profile%h_qc (k))
      col = col + 1; if (.not. is_missing(profile%h_cr (k))) call odbql_bind_double(odb_stmt, col, dble(profile%h_cr (k)))
      col = col + 1; if (.not. is_missing(profile%ta   (k))) call odbql_bind_double(odb_stmt, col, dble(profile%ta   (k)))
      col = col + 1; if (.not. is_missing(profile%ta_qc(k))) call odbql_bind_int   (odb_stmt, col,      profile%ta_qc(k))
      col = col + 1; if (.not. is_missing(profile%ta_cr(k))) call odbql_bind_double(odb_stmt, col, dble(profile%ta_cr(k)))
      col = col + 1; if (.not. is_missing(profile%td   (k))) call odbql_bind_double(odb_stmt, col, dble(profile%td   (k)))
      col = col + 1; if (.not. is_missing(profile%sh   (k))) call odbql_bind_double(odb_stmt, col, dble(profile%sh   (k)))
      col = col + 1; if (.not. is_missing(profile%sh_qc(k))) call odbql_bind_int   (odb_stmt, col,      profile%sh_qc(k))
      col = col + 1; if (.not. is_missing(profile%sh_cr(k))) call odbql_bind_double(odb_stmt, col, dble(profile%sh_cr(k)))
      col = col + 1; if (.not. is_missing(profile%ua   (k))) call odbql_bind_double(odb_stmt, col, dble(profile%ua   (k)))
      col = col + 1; if (.not. is_missing(profile%ua_qc(k))) call odbql_bind_int   (odb_stmt, col,      profile%ua_qc(k))
      col = col + 1; if (.not. is_missing(profile%ua_cr(k))) call odbql_bind_double(odb_stmt, col, dble(profile%ua_cr(k)))
      col = col + 1; if (.not. is_missing(profile%va   (k))) call odbql_bind_double(odb_stmt, col, dble(profile%va   (k)))
      col = col + 1; if (.not. is_missing(profile%va_qc(k))) call odbql_bind_int   (odb_stmt, col,      profile%va_qc(k))
      col = col + 1; if (.not. is_missing(profile%va_cr(k))) call odbql_bind_double(odb_stmt, col, dble(profile%va_cr(k)))
      call odbql_step(odb_stmt)
      call odb_all_bind_null(odb_stmt, col)
    end do

  end subroutine write_profile

end module raob_odb_mod
