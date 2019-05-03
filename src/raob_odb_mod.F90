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
      'pressure REAL, '                     // &
      'pressure_qc INTEGER, '               // &
      'pressure_correct REAL, '             // &
      'height REAL, '                       // &
      'height_qc INTEGER, '                 // &
      'height_correct REAL, '               // &
      'temperature REAL, '                  // &
      'temperature_qc INTEGER, '            // &
      'temperature_correct REAL, '          // &
      'dewpoint REAL, '                     // &
      'specific_humidity REAL, '            // &
      'specific_humidity_qc INTEGER, '      // &
      'specific_humidity_correct REAL, '    // &
      'wind_u REAL, '                       // &
      'wind_u_correct REAL, '               // &
      'wind_v REAL, '                       // &
      'wind_v_correct REAL, '               // &
      'wind_qc INTEGER'                     // &
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
      'pressure, '                          // &
      'pressure_qc, '                       // &
      'pressure_correct, '                  // &
      'height, '                            // &
      'height_qc, '                         // &
      'height_correct, '                    // &
      'temperature, '                       // &
      'temperature_qc, '                    // &
      'temperature_correct, '               // &
      'dewpoint, '                          // &
      'specific_humidity, '                 // &
      'specific_humidity_qc, '              // &
      'specific_humidity_correct, '         // &
      'wind_u, '                            // &
      'wind_u_correct, '                    // &
      'wind_v, '                            // &
      'wind_v_correct, '                    // &
      'wind_qc'                             // &
      ') VALUES (' // trim(odb_values_placeholder(27)) // ');', -1, odb_stmt, odb_unparsed_sql)

    record_iterator = linked_list_iterator(records)
    do while (.not. record_iterator%ended())
      select type (record => record_iterator%value)
      type is (raob_record_type)
        ! Surface level
        col = 0
        col = col + 1; call odbql_bind_text  (odb_stmt, col, record%station%name, len_trim(record%station%name))
        col = col + 1;
        col = col + 1;
        col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%lon))
        col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%lat))
        col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%z))
        col = col + 1; call odbql_bind_text  (odb_stmt, col, 'sfc', 3)
        col = col + 1; call odbql_bind_int   (odb_stmt, col, to_integer(record%time%format('%Y%m%d')))
        col = col + 1; call odbql_bind_int   (odb_stmt, col, to_integer(record%time%format('%H%M%S')))
        col = col + 1; if (.not. is_missing(record%sfc_pressure))             call odbql_bind_double(odb_stmt, col, dble(record%sfc_pressure))
        col = col + 1; if (.not. is_missing(record%sfc_pressure_qc))          call odbql_bind_int   (odb_stmt, col, record%sfc_pressure_qc)
        col = col + 1; if (.not. is_missing(record%sfc_pressure_correct))     call odbql_bind_double(odb_stmt, col, dble(record%sfc_pressure_correct))
        col = col + 1;
        col = col + 1;
        col = col + 1;
        col = col + 1; if (.not. is_missing(record%sfc_temperature))          call odbql_bind_double(odb_stmt, col, dble(record%sfc_temperature))
        col = col + 1; if (.not. is_missing(record%sfc_temperature_qc))       call odbql_bind_int   (odb_stmt, col, record%sfc_temperature_qc)
        col = col + 1; if (.not. is_missing(record%sfc_temperature_correct))  call odbql_bind_double(odb_stmt, col, dble(record%sfc_temperature_correct))
        col = col + 1; if (.not. is_missing(record%sfc_dewpoint))             call odbql_bind_double(odb_stmt, col, dble(record%sfc_dewpoint))
        col = col + 1; if (.not. is_missing(record%sfc_specific_humidity))    call odbql_bind_double(odb_stmt, col, dble(record%sfc_specific_humidity))
        col = col + 1; if (.not. is_missing(record%sfc_specific_humidity_qc)) call odbql_bind_double(odb_stmt, col, dble(record%sfc_specific_humidity_qc))
        col = col + 1; if (.not. is_missing(record%sfc_specific_humidity_correct)) call odbql_bind_double(odb_stmt, col, dble(record%sfc_specific_humidity_correct))
        col = col + 1; if (.not. is_missing(record%sfc_wind_u))               call odbql_bind_double(odb_stmt, col, dble(record%sfc_wind_u))
        col = col + 1; if (.not. is_missing(record%sfc_wind_u_correct))       call odbql_bind_double(odb_stmt, col, dble(record%sfc_wind_u_correct))
        col = col + 1; if (.not. is_missing(record%sfc_wind_v))               call odbql_bind_double(odb_stmt, col, dble(record%sfc_wind_v))
        col = col + 1; if (.not. is_missing(record%sfc_wind_v_correct))       call odbql_bind_double(odb_stmt, col, dble(record%sfc_wind_v_correct))
        col = col + 1; if (.not. is_missing(record%sfc_wind_qc))              call odbql_bind_double(odb_stmt, col, dble(record%sfc_wind_qc))
        call odbql_step(odb_stmt)
        call odb_all_bind_null(odb_stmt, col)
        ! Mandatory levels
        do k = 1, record%man%num_level
          col = 0
          col = col + 1; call odbql_bind_text  (odb_stmt, col, record%station%name, len_trim(record%station%name))
          col = col + 1;
          col = col + 1;
          col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%lon))
          col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%lat))
          col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%z))
          col = col + 1; call odbql_bind_text  (odb_stmt, col, 'man', 3)
          col = col + 1; call odbql_bind_int   (odb_stmt, col, to_integer(record%time%format('%Y%m%d')))
          col = col + 1; call odbql_bind_int   (odb_stmt, col, to_integer(record%time%format('%H%M%S')))
          col = col + 1; if (.not. is_missing(record%man%pressure(k)))                  call odbql_bind_double(odb_stmt, col, dble(record%man%pressure(k)))
          col = col + 1; if (.not. is_missing(record%man%pressure_qc(k)))               call odbql_bind_int   (odb_stmt, col, record%man%pressure_qc(k))
          col = col + 1; if (.not. is_missing(record%man%pressure_correct(k)))          call odbql_bind_double(odb_stmt, col, dble(record%man%pressure_correct(k)))
          col = col + 1; if (.not. is_missing(record%man%height(k)))                    call odbql_bind_double(odb_stmt, col, dble(record%man%height(k)))
          col = col + 1; if (.not. is_missing(record%man%height_qc(k)))                 call odbql_bind_int   (odb_stmt, col, record%man%height_qc(k))
          col = col + 1; if (.not. is_missing(record%man%height_correct(k)))            call odbql_bind_double(odb_stmt, col, dble(record%man%height_correct(k)))
          col = col + 1; if (.not. is_missing(record%man%temperature(k)))               call odbql_bind_double(odb_stmt, col, dble(record%man%temperature(k)))
          col = col + 1; if (.not. is_missing(record%man%temperature_qc(k)))            call odbql_bind_int   (odb_stmt, col, record%man%temperature_qc(k))
          col = col + 1; if (.not. is_missing(record%man%temperature_correct(k)))       call odbql_bind_double(odb_stmt, col, dble(record%man%temperature_correct(k)))
          col = col + 1; if (.not. is_missing(record%man%dewpoint(k)))                  call odbql_bind_double(odb_stmt, col, dble(record%man%dewpoint(k)))
          col = col + 1; if (.not. is_missing(record%man%specific_humidity(k)))         call odbql_bind_double(odb_stmt, col, dble(record%man%specific_humidity(k)))
          col = col + 1; if (.not. is_missing(record%man%specific_humidity_qc(k)))      call odbql_bind_int   (odb_stmt, col, record%man%specific_humidity_qc(k))
          col = col + 1; if (.not. is_missing(record%man%specific_humidity_correct(k))) call odbql_bind_double(odb_stmt, col, dble(record%man%specific_humidity_correct(k)))
          col = col + 1; if (.not. is_missing(record%man%wind_u(k)))                    call odbql_bind_double(odb_stmt, col, dble(record%man%wind_u(k)))
          col = col + 1; if (.not. is_missing(record%man%wind_u_correct(k)))            call odbql_bind_double(odb_stmt, col, dble(record%man%wind_u_correct(k)))
          col = col + 1; if (.not. is_missing(record%man%wind_v(k)))                    call odbql_bind_double(odb_stmt, col, dble(record%man%wind_v(k)))
          col = col + 1; if (.not. is_missing(record%man%wind_v_correct(k)))            call odbql_bind_double(odb_stmt, col, dble(record%man%wind_v_correct(k)))
          col = col + 1; if (.not. is_missing(record%man%wind_qc(k)))                   call odbql_bind_double(odb_stmt, col, dble(record%man%wind_qc(k)))
          call odbql_step(odb_stmt)
          call odb_all_bind_null(odb_stmt, col)
        end do
        ! Significant temperature levels
        do k = 1, record%sigt%num_level
          col = 0
          col = col + 1; call odbql_bind_text  (odb_stmt, col, record%station%name, len_trim(record%station%name))
          col = col + 1;
          col = col + 1;
          col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%lon))
          col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%lat))
          col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%z))
          col = col + 1; call odbql_bind_text  (odb_stmt, col, 'sigt', 4)
          col = col + 1; call odbql_bind_int   (odb_stmt, col, to_integer(record%time%format('%Y%m%d')))
          col = col + 1; call odbql_bind_int   (odb_stmt, col, to_integer(record%time%format('%H%M%S')))
          col = col + 1; if (.not. is_missing(record%sigt%pressure(k)))                  call odbql_bind_double(odb_stmt, col, dble(record%sigt%pressure(k)))
          col = col + 1; if (.not. is_missing(record%sigt%pressure_qc(k)))               call odbql_bind_int   (odb_stmt, col, record%sigt%pressure_qc(k))
          col = col + 1; if (.not. is_missing(record%sigt%pressure_correct(k)))          call odbql_bind_double(odb_stmt, col, dble(record%sigt%pressure_correct(k)))
          col = col + 1; if (.not. is_missing(record%sigt%height(k)))                    call odbql_bind_double(odb_stmt, col, dble(record%sigt%height(k)))
          col = col + 1; if (.not. is_missing(record%sigt%height_qc(k)))                 call odbql_bind_int   (odb_stmt, col, record%sigt%height_qc(k))
          col = col + 1; if (.not. is_missing(record%sigt%height_correct(k)))            call odbql_bind_double(odb_stmt, col, dble(record%sigt%height_correct(k)))
          col = col + 1; if (.not. is_missing(record%sigt%temperature(k)))               call odbql_bind_double(odb_stmt, col, dble(record%sigt%temperature(k)))
          col = col + 1; if (.not. is_missing(record%sigt%temperature_qc(k)))            call odbql_bind_int   (odb_stmt, col, record%sigt%temperature_qc(k))
          col = col + 1; if (.not. is_missing(record%sigt%temperature_correct(k)))       call odbql_bind_double(odb_stmt, col, dble(record%sigt%temperature_correct(k)))
          col = col + 1; if (.not. is_missing(record%sigt%dewpoint(k)))                  call odbql_bind_double(odb_stmt, col, dble(record%sigt%dewpoint(k)))
          col = col + 1; if (.not. is_missing(record%sigt%specific_humidity(k)))         call odbql_bind_double(odb_stmt, col, dble(record%sigt%specific_humidity(k)))
          col = col + 1; if (.not. is_missing(record%sigt%specific_humidity_qc(k)))      call odbql_bind_int   (odb_stmt, col, record%sigt%specific_humidity_qc(k))
          col = col + 1; if (.not. is_missing(record%sigt%specific_humidity_correct(k))) call odbql_bind_double(odb_stmt, col, dble(record%sigt%specific_humidity_correct(k)))
          col = col + 1; if (.not. is_missing(record%sigt%wind_u(k)))                    call odbql_bind_double(odb_stmt, col, dble(record%sigt%wind_u(k)))
          col = col + 1; if (.not. is_missing(record%sigt%wind_u_correct(k)))            call odbql_bind_double(odb_stmt, col, dble(record%sigt%wind_u_correct(k)))
          col = col + 1; if (.not. is_missing(record%sigt%wind_v(k)))                    call odbql_bind_double(odb_stmt, col, dble(record%sigt%wind_v(k)))
          col = col + 1; if (.not. is_missing(record%sigt%wind_v_correct(k)))            call odbql_bind_double(odb_stmt, col, dble(record%sigt%wind_v_correct(k)))
          col = col + 1; if (.not. is_missing(record%sigt%wind_qc(k)))                   call odbql_bind_int   (odb_stmt, col, record%sigt%wind_qc(k))
          call odbql_step(odb_stmt)
          call odb_all_bind_null(odb_stmt, col)
        end do
        ! Significant wind levels
        do k = 1, record%sigw%num_level
          col = 0
          col = col + 1; call odbql_bind_text  (odb_stmt, col, record%station%name, len_trim(record%station%name))
          col = col + 1;
          col = col + 1;
          col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%lon))
          col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%lat))
          col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%z))
          col = col + 1; call odbql_bind_text  (odb_stmt, col, 'sigw', 4)
          col = col + 1; call odbql_bind_int   (odb_stmt, col, to_integer(record%time%format('%Y%m%d')))
          col = col + 1; call odbql_bind_int   (odb_stmt, col, to_integer(record%time%format('%H%M%S')))
          col = col + 1; if (.not. is_missing(record%sigw%pressure(k)))                  call odbql_bind_double(odb_stmt, col, dble(record%sigw%pressure(k)))
          col = col + 1; if (.not. is_missing(record%sigw%pressure_qc(k)))               call odbql_bind_int   (odb_stmt, col, record%sigw%pressure_qc(k))
          col = col + 1; if (.not. is_missing(record%sigw%pressure_correct(k)))          call odbql_bind_double(odb_stmt, col, dble(record%sigw%pressure_correct(k)))
          col = col + 1; if (.not. is_missing(record%sigw%height(k)))                    call odbql_bind_double(odb_stmt, col, dble(record%sigw%height(k)))
          col = col + 1; if (.not. is_missing(record%sigw%height_qc(k)))                 call odbql_bind_int   (odb_stmt, col, record%sigw%height_qc(k))
          col = col + 1; if (.not. is_missing(record%sigw%height_correct(k)))            call odbql_bind_double(odb_stmt, col, dble(record%sigw%height_correct(k)))
          col = col + 1; if (.not. is_missing(record%sigw%temperature(k)))               call odbql_bind_double(odb_stmt, col, dble(record%sigw%temperature(k)))
          col = col + 1; if (.not. is_missing(record%sigw%temperature_qc(k)))            call odbql_bind_int   (odb_stmt, col, record%sigw%temperature_qc(k))
          col = col + 1; if (.not. is_missing(record%sigw%temperature_correct(k)))       call odbql_bind_double(odb_stmt, col, dble(record%sigw%temperature_correct(k)))
          col = col + 1; if (.not. is_missing(record%sigw%dewpoint(k)))                  call odbql_bind_double(odb_stmt, col, dble(record%sigw%dewpoint(k)))
          col = col + 1; if (.not. is_missing(record%sigw%specific_humidity(k)))         call odbql_bind_double(odb_stmt, col, dble(record%sigw%specific_humidity(k)))
          col = col + 1; if (.not. is_missing(record%sigw%specific_humidity_qc(k)))      call odbql_bind_int   (odb_stmt, col, record%sigw%specific_humidity_qc(k))
          col = col + 1; if (.not. is_missing(record%sigw%specific_humidity_correct(k))) call odbql_bind_double(odb_stmt, col, dble(record%sigw%specific_humidity_correct(k)))
          col = col + 1; if (.not. is_missing(record%sigw%wind_u(k)))                    call odbql_bind_double(odb_stmt, col, dble(record%sigw%wind_u(k)))
          col = col + 1; if (.not. is_missing(record%sigw%wind_u_correct(k)))            call odbql_bind_double(odb_stmt, col, dble(record%sigw%wind_u_correct(k)))
          col = col + 1; if (.not. is_missing(record%sigw%wind_v(k)))                    call odbql_bind_double(odb_stmt, col, dble(record%sigw%wind_v(k)))
          col = col + 1; if (.not. is_missing(record%sigw%wind_v_correct(k)))            call odbql_bind_double(odb_stmt, col, dble(record%sigw%wind_v_correct(k)))
          col = col + 1; if (.not. is_missing(record%sigw%wind_qc(k)))                   call odbql_bind_int   (odb_stmt, col, record%sigw%wind_qc(k))
          call odbql_step(odb_stmt)
          call odb_all_bind_null(odb_stmt, col)
        end do
        ! Tropopause levels
        do k = 1, record%trop%num_level
          col = 0
          col = col + 1; call odbql_bind_text  (odb_stmt, col, record%station%name, len_trim(record%station%name))
          col = col + 1;
          col = col + 1;
          col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%lon))
          col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%lat))
          col = col + 1; call odbql_bind_double(odb_stmt, col, dble(record%station%z))
          col = col + 1; call odbql_bind_text  (odb_stmt, col, 'trop', 4)
          col = col + 1; call odbql_bind_int   (odb_stmt, col, to_integer(record%time%format('%Y%m%d')))
          col = col + 1; call odbql_bind_int   (odb_stmt, col, to_integer(record%time%format('%H%M%S')))
          col = col + 1; if (.not. is_missing(record%trop%pressure(k)))                  call odbql_bind_double(odb_stmt, col, dble(record%trop%pressure(k)))
          col = col + 1; if (.not. is_missing(record%trop%pressure_qc(k)))               call odbql_bind_int   (odb_stmt, col, record%trop%pressure_qc(k))
          col = col + 1; if (.not. is_missing(record%trop%pressure_correct(k)))          call odbql_bind_double(odb_stmt, col, dble(record%trop%pressure_correct(k)))
          col = col + 1; if (.not. is_missing(record%trop%height(k)))                    call odbql_bind_double(odb_stmt, col, dble(record%trop%height(k)))
          col = col + 1; if (.not. is_missing(record%trop%height_qc(k)))                 call odbql_bind_int   (odb_stmt, col, record%trop%height_qc(k))
          col = col + 1; if (.not. is_missing(record%trop%height_correct(k)))            call odbql_bind_double(odb_stmt, col, dble(record%trop%height_correct(k)))
          col = col + 1; if (.not. is_missing(record%trop%temperature(k)))               call odbql_bind_double(odb_stmt, col, dble(record%trop%temperature(k)))
          col = col + 1; if (.not. is_missing(record%trop%temperature_qc(k)))            call odbql_bind_int   (odb_stmt, col, record%trop%temperature_qc(k))
          col = col + 1; if (.not. is_missing(record%trop%temperature_correct(k)))       call odbql_bind_double(odb_stmt, col, dble(record%trop%temperature_correct(k)))
          col = col + 1; if (.not. is_missing(record%trop%dewpoint(k)))                  call odbql_bind_double(odb_stmt, col, dble(record%trop%dewpoint(k)))
          col = col + 1; if (.not. is_missing(record%trop%specific_humidity(k)))         call odbql_bind_double(odb_stmt, col, dble(record%trop%specific_humidity(k)))
          col = col + 1; if (.not. is_missing(record%trop%specific_humidity_qc(k)))      call odbql_bind_int   (odb_stmt, col, record%trop%specific_humidity_qc(k))
          col = col + 1; if (.not. is_missing(record%trop%specific_humidity_correct(k))) call odbql_bind_double(odb_stmt, col, dble(record%trop%specific_humidity_correct(k)))
          col = col + 1; if (.not. is_missing(record%trop%wind_u(k)))                    call odbql_bind_double(odb_stmt, col, dble(record%trop%wind_u(k)))
          col = col + 1; if (.not. is_missing(record%trop%wind_u_correct(k)))            call odbql_bind_double(odb_stmt, col, dble(record%trop%wind_u_correct(k)))
          col = col + 1; if (.not. is_missing(record%trop%wind_v(k)))                    call odbql_bind_double(odb_stmt, col, dble(record%trop%wind_v(k)))
          col = col + 1; if (.not. is_missing(record%trop%wind_v_correct(k)))            call odbql_bind_double(odb_stmt, col, dble(record%trop%wind_v_correct(k)))
          col = col + 1; if (.not. is_missing(record%trop%wind_qc(k)))                   call odbql_bind_int   (odb_stmt, col, record%trop%wind_qc(k))
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
