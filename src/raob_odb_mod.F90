module raob_odb_mod

  use hash_table_mod
  use linked_list_mod
  use raob_mod
  use odbql_wrappers

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
    type(odbql_value) odb_value
    character(100) odb_unparsed_sql

    character(30) str
    type(linked_list_iterator_type) record_iterator
    type(hash_table_iterator_type) level_iterator

    if (file_path == '') file_path = 'raob.odb'

    ! Write ODB file.
    call odbql_open('', odb_db)
    call odbql_prepare_v2(odb_db, 'CREATE TABLE raob AS (' // &
      'station_name STRING, ' // &                !  1
      'lon REAL, ' // &                           !  2
      'lat REAL, ' // &                           !  3
      'z REAL, ' // &                             !  4
      'date STRING, ' // &                        !  5
      'time STRING, ' // &                        !  6
      'height REAL, ' // &                        !  7
      'pressure REAL, ' // &                      !  8
      'temperature REAL, ' // &                   !  9
      'dewpoint REAL, ' // &                      ! 10
      'wind_direction REAL, ' // &                ! 11
      'wind_speed REAL, ' // &                    ! 12
      'wind_u REAL, ' // &                        ! 13
      'wind_v REAL, ' // &                        ! 14
      'specific_humidity REAL, ' // &             ! 15
      'relative_humidity REAL' // &               ! 16
      ') ON "' // trim(file_path) // '";', -1, odb_stmt, odb_unparsed_sql)
    call odbql_prepare_v2(odb_db, 'INSERT INTO raob (' // &
      'station_name, ' // &                       !  1
      'lon, ' // &                                !  2
      'lat, ' // &                                !  3
      'z, ' // &                                  !  4
      'date, ' // &                               !  5
      'time, ' // &                               !  6
      'height, ' // &                             !  7
      'pressure, ' // &                           !  8
      'temperature, ' // &                        !  9
      'dewpoint, ' // &                           ! 10
      'wind_direction, ' // &                     ! 11
      'wind_speed, ' // &                         ! 12
      'wind_u, ' // &                             ! 13
      'wind_v, ' // &                             ! 14
      'specific_humidity,' // &                   ! 15
      'relative_humidity' // &                    ! 16
      ') VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);', -1, odb_stmt, odb_unparsed_sql)

    record_iterator = linked_list_iterator(records)
    do while (.not. record_iterator%ended())
      select type (record => record_iterator%value)
      type is (raob_read_record_type)
        level_iterator = hash_table_iterator(record%snd_man_pressure)
        do while (.not. level_iterator%ended())
          call odbql_bind_text  (odb_stmt,    1, record%station%name, len_trim(record%station%name))
          call odbql_bind_double(odb_stmt,    2, dble(record%station%lon))
          call odbql_bind_double(odb_stmt,    3, dble(record%station%lat))
          call odbql_bind_double(odb_stmt,    4, dble(record%station%z))
          str = record%time%format('%Y%m%d')
          call odbql_bind_text  (odb_stmt,    5, trim(str), len_trim(str))
          str = record%time%format('%H%M%S')
          call odbql_bind_text  (odb_stmt,    6, trim(str), len_trim(str))
          ! height (m)
          select type (value => record%snd_man_height%value(level_iterator%key))
          type is (real)
            call odbql_bind_double(odb_stmt,  7, dble(value))
          class default
            call odbql_bind_double(odb_stmt,  7, dble(real_missing_value))
          end select
          ! pressure (Pa)
          select type (value => level_iterator%value)
          type is (real)
            call odbql_bind_double(odb_stmt,  8, dble(value))
          class default
            call odbql_bind_double(odb_stmt,  8, dble(real_missing_value))
          end select
          ! temperature (degC)
          select type (value => record%snd_man_temperature%value(level_iterator%key))
          type is (real)
            call odbql_bind_double(odb_stmt,  9, dble(value))
          class default
            call odbql_bind_double(odb_stmt,  9, dble(real_missing_value))
          end select
          ! dewpoint (degC)
          select type (value => record%snd_man_dewpoint%value(level_iterator%key))
          type is (real)
            call odbql_bind_double(odb_stmt, 10, dble(value))
          class default
            call odbql_bind_double(odb_stmt, 10, dble(real_missing_value))
          end select
          ! wind direction (degree)
          select type (value => record%snd_man_wind_direction%value(level_iterator%key))
          type is (real)
            call odbql_bind_double(odb_stmt, 11, dble(value))
          class default
            call odbql_bind_double(odb_stmt, 11, dble(real_missing_value))
          end select
          ! wind speed (m/s)
          select type (value => record%snd_man_wind_speed%value(level_iterator%key))
          type is (real)
            call odbql_bind_double(odb_stmt, 12, dble(value))
          class default
            call odbql_bind_double(odb_stmt, 12, dble(real_missing_value))
          end select
          ! wind u component (m/s)
          select type (value => record%snd_man_wind_u%value(level_iterator%key))
          type is (real)
            call odbql_bind_double(odb_stmt, 13, dble(value))
          class default
            call odbql_bind_double(odb_stmt, 13, dble(real_missing_value))
          end select
          ! wind v component (m/s)
          select type (value => record%snd_man_wind_v%value(level_iterator%key))
          type is (real)
            call odbql_bind_double(odb_stmt, 14, dble(value))
          class default
            call odbql_bind_double(odb_stmt, 14, dble(real_missing_value))
          end select
          ! specific humidity (Mg/Kg)
          select type (value => record%snd_man_specific_humidity%value(level_iterator%key))
          type is (real)
            call odbql_bind_double(odb_stmt, 15, dble(value))
          class default
            call odbql_bind_double(odb_stmt, 15, dble(real_missing_value))
          end select
          ! relative humidity (%)
          select type (value => record%snd_man_relative_humidity%value(level_iterator%key))
          type is (real)
            call odbql_bind_double(odb_stmt, 16, dble(value))
          class default
            call odbql_bind_double(odb_stmt, 16, dble(real_missing_value))
          end select
          call odbql_step(odb_stmt)
          call level_iterator%next()
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
