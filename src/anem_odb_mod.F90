module anem_odb_mod

  use odbql_wrappers
  use container
  use flogger
  use data_translators_utils_mod
  use cli_mod
  use anem_mod

  private

  public anem_odb_write
  public anem_odb_read

contains

  subroutine anem_odb_write(file_path, towers, records)

    character(*), intent(inout) :: file_path
    type(hash_table_type), intent(inout) :: towers
    type(linked_list_type), intent(inout) :: records

    ! ODB variables
    type(odbql) odb_db
    type(odbql_stmt) odb_stmt
    character(100) :: odb_unparsed_sql = ''

    integer col
    type(linked_list_iterator_type) record_iterator

    if (file_path == '') file_path = 'anem.odb'

    ! Write ODB file.
    call odbql_open('', odb_db)
    call odbql_prepare_v2(odb_db, 'CREATE TABLE anem AS (' // &
      'sid STRING, '           // &
      'lon REAL, '             // &
      'lat REAL, '             // &
      'alt REAL, '             // &
      'date INTEGER, '         // &
      'time INTEGER, '         // &
      'hgt REAL, '             // &
      'ta REAL, '              // &
      'p REAL, '               // &
      'rh REAL, '              // &
      'ws REAL, '              // &
      'ws_min REAL, '          // &
      'ws_max REAL, '          // &
      'ws_std REAL, '          // &
      'wd REAL, '              // &
      'ua REAL, '              // &
      'va REAL'                // &
      ') ON "' // trim(file_path) // '";', -1, odb_stmt, odb_unparsed_sql)
    call odbql_prepare_v2(odb_db, 'INSERT INTO anem (' // &
      'sid, '                  // &
      'lon, '                  // &
      'lat, '                  // &
      'alt, '                  // &
      'date, '                 // &
      'time, '                 // &
      'hgt, '                  // &
      'ta, '                   // &
      'p, '                    // &
      'rh, '                   // &
      'ws, '                   // &
      'ws_min, '               // &
      'ws_max, '               // &
      'ws_std, '               // &
      'wd, '                   // &
      'ua, '                   // &
      'va'                     // &
      ') VALUES (' // odb_values_placeholder(17) // ');', -1, odb_stmt, odb_unparsed_sql)

    record_iterator = linked_list_iterator(records)
    do while (.not. record_iterator%ended())
      select type (record => record_iterator%value)
      type is (anem_record_type)
        do k = 1, size(record%h)
          col = 0
          col = col + 1; call odbql_bind_text  (odb_stmt, col, record%tower%name, len_trim(record%tower%name))
          col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%tower%lon))
          col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%tower%lat))
          col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%tower%z))
          col = col + 1; call odbql_bind_int   (odb_stmt, col, to_int(record%time%format('%Y%m%d')))
          col = col + 1; call odbql_bind_int   (odb_stmt, col, to_int(record%time%format('%H%M%S')))
          col = col + 1; if (.not. is_missing(record%h     (k))) call odbql_bind_double(odb_stmt, col, dble(add(record%h(k), record%tower%z)))
          col = col + 1; if (.not. is_missing(record%ta_avg(k))) call odbql_bind_double(odb_stmt, col, dble(record%ta_avg(k)))
          col = col + 1; if (.not. is_missing(record%p_avg (k))) call odbql_bind_double(odb_stmt, col, dble(record%p_avg (k)))
          col = col + 1; if (.not. is_missing(record%rh    (k))) call odbql_bind_double(odb_stmt, col, dble(record%rh    (k)))
          col = col + 1; if (.not. is_missing(record%ws_avg(k))) call odbql_bind_double(odb_stmt, col, dble(record%ws_avg(k)))
          col = col + 1; if (.not. is_missing(record%ws_min(k))) call odbql_bind_double(odb_stmt, col, dble(record%ws_min(k)))
          col = col + 1; if (.not. is_missing(record%ws_max(k))) call odbql_bind_double(odb_stmt, col, dble(record%ws_max(k)))
          col = col + 1; if (.not. is_missing(record%ws_std(k))) call odbql_bind_double(odb_stmt, col, dble(record%ws_std(k)))
          col = col + 1; if (.not. is_missing(record%wd_avg(k))) call odbql_bind_double(odb_stmt, col, dble(record%wd_avg(k)))
          col = col + 1; if (.not. is_missing(record%ua_avg(k))) call odbql_bind_double(odb_stmt, col, dble(record%ua_avg(k)))
          col = col + 1; if (.not. is_missing(record%va_avg(k))) call odbql_bind_double(odb_stmt, col, dble(record%va_avg(k)))
          call odbql_step(odb_stmt)
          call odb_all_bind_null(odb_stmt, col)
        end do
      class default
        call log_error('Unknown record in the list!')
      end select
      call record_iterator%next()
    end do

    call odbql_finalize(odb_stmt)
    call odbql_close(odb_db)

  end subroutine anem_odb_write

  subroutine anem_odb_read(file_path, towers, records)

    character(*), intent(inout) :: file_path
    type(hash_table_type), intent(inout) :: towers
    type(linked_list_type), intent(inout) :: records

    ! ODB variables
    type(odbql) odb_db
    type(odbql_stmt) odb_stmt
    character(100) :: odb_unparsed_sql = ''
    integer(kind=c_int) ierr
    integer col
    character(30) col_name
    type(odbql_value) col_val

    type(anem_tower_type), pointer :: tower
    type(anem_record_type) record
    character(30) sid
    type(datetime_type) time
    real lon, lat, alt, h, p_avg, ta_avg, ua_avg, va_avg, ws_avg, ws_std, ws_min, ws_max, wd_avg, rh
    integer date_int, time_int

    call odbql_open(trim(file_path), odb_db)
    if (cli_start_time%year /= 0) then
      call odbql_prepare_v2(odb_db, 'SELECT * FROM "' // trim(file_path) // '" WHERE ' // &
        'tdiff(date, time, ' // trim(cli_start_time%format('%Y%m%d')) // ', '          // &
                                trim(cli_start_time%format('%H%M%S')) // ') >= 0 and ' // &
        'tdiff(date, time, ' // trim(cli_end_time  %format('%Y%m%d')) // ', '          // &
                                trim(cli_end_time  %format('%H%M%S')) // ') <= 0;',       &
        -1, odb_stmt, odb_unparsed_sql)
    else
      call odbql_prepare_v2(odb_db, 'SELECT * FROM "' // trim(file_path) // '";', -1, odb_stmt, odb_unparsed_sql)
    end if

    do
      call odbql_step(odb_stmt, ierr)
      if (ierr == ODBQL_DONE) exit
      if (ierr /= ODBQL_ROW) then
        call log_error('Failed to read ODB file ' // trim(file_path) // '!')
      end if
      do col = 1, odbql_column_count(odb_stmt)
        call odbql_column_name(odb_stmt, col, col_name)
        col_val = odbql_column_value(odb_stmt, col)
        select case (col_name)
        case ('sid@anem')
          call odbql_column_text(odb_stmt, col, sid)
          if (towers%hashed(sid)) then
            select type (value => towers%value(sid))
            type is (anem_tower_type)
              tower => value
            end select
          else
            allocate(tower)
          end if
        case ('lon@anem')
          lon = odbql_value_double(col_val)
        case ('lat@anem')
          lat = odbql_value_double(col_val)
        case ('alt@anem')
          alt = odbql_value_double(col_val)
        case ('date@anem')
          date_int = odbql_value_int(col_val)
        case ('time@anem')
          time_int = odbql_value_int(col_val)
        case ('hgt@anem')
          h = odbql_value_double(col_val)
        case ('ta@anem')
          !ta_avg = odbql_value_double(col_val)
        case ('p@anem')
          p_avg = odbql_value_double(col_val)
        case ('rh@anem')
          rh = odbql_value_double(col_val)
        case ('ws@anem')
          ws_avg = odbql_value_double(col_val)
        case ('ws_min@anem')
          ws_min = odbql_value_double(col_val)
        case ('ws_max@anem')
          ws_max = odbql_value_double(col_val)
        case ('ws_std@anem')
          ws_std = odbql_value_double(col_val)
        case ('wd@anem')
          wd_avg = odbql_value_double(col_val)
        case ('ua@anem')
          ua_avg = odbql_value_double(col_val)
        case ('va@anem')
          va_avg = odbql_value_double(col_val)
        end select
      end do
      if (is_missing(tower%lon)) then
        call tower%init(sid, lon, lat, alt)
        tower%seq_id = towers%size
        call towers%insert(tower%name, tower)
        print *, loc(tower)
      end if
      exit
    end do

    call odbql_finalize(odb_stmt)
    call odbql_close(odb_db)

  end subroutine anem_odb_read

end module anem_odb_mod
