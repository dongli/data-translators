module amdar_odb_mod

  use hash_table_mod
  use linked_list_mod
  use amdar_mod
  use odbql_wrappers

  implicit none

  private

  public amdar_odb_write

contains


  subroutine amdar_odb_write(file_path)

    character(*), intent(inout) :: file_path

    ! ODB variables
    type(odbql) odb_db
    type(odbql_stmt) odb_stmt
    type(odbql_value) odb_value
    character(100) odb_unparsed_sql

    character(30) str
    type(linked_list_iterator_type) record_iterator

    if (file_path == '') file_path = 'amdar.odb'

    ! Write ODB file.
    call odbql_open('', odb_db)
    call odbql_prepare_v2(odb_db, 'CREATE TABLE amdar AS (' // &
      'flight_name STRING, ' // &                   !  1
      'flight_number STRING, ' // &                 !  2
      'lon REAL, ' // &                             !  3
      'lat REAL, ' // &                             !  4
      'z REAL, ' // &                               !  5
      'date STRING, ' // &                          !  6
      'time STRING, ' // &                          !  7
      'pressure REAL, ' // &                        !  8
      'temperature REAL, ' // &                     !  9
      'wind_u REAL, ' // &                          ! 10
      'wind_v REAL, ' // &                          ! 11
      'turbulence_index INTEGER, ' // &             ! 12
      'specific_humidity REAL) ON "' // &           ! 13
      trim(file_path) // '";', -1, odb_stmt, odb_unparsed_sql)
    call odbql_prepare_v2(odb_db, 'INSERT INTO amdar (' // &
      'flight_name, ' // &                          !  1
      'flight_number, ' // &                        !  2
      'lon, ' // &                                  !  3
      'lat, ' // &                                  !  4
      'z, ' // &                                    !  5
      'date, ' // &                                 !  6
      'time, ' // &                                 !  7
      'pressure, ' // &                             !  8
      'temperature, ' // &                          !  9
      'wind_u, ' // &                               ! 10
      'wind_v, ' // &                               ! 11
      'turbulence_index, ' // &                     ! 12
      'specific_humidity' // &                      ! 13
      ') VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);', -1, odb_stmt, odb_unparsed_sql)

    record_iterator = linked_list_iterator(records)
    do while (.not. record_iterator%ended())
      select type (record => record_iterator%value)
      type is (amdar_record_type)
        call odbql_bind_text  (odb_stmt,  1, record%flight%name, len_trim(record%flight%name))
        call odbql_bind_text  (odb_stmt,  2, record%flight%number, len_trim(record%flight%number))
        call odbql_bind_double(odb_stmt,  3, dble(record%lon))
        call odbql_bind_double(odb_stmt,  4, dble(record%lat))
        call odbql_bind_double(odb_stmt,  5, dble(record%z))
        str = record%time%format('%Y%m%d')
        call odbql_bind_text  (odb_stmt,  6, trim(str), len_trim(str))
        str = record%time%format('%H%M%S')
        call odbql_bind_text  (odb_stmt,  7, trim(str), len_trim(str))
        call odbql_bind_double(odb_stmt,  8, dble(record%amdar_pressure))
        call odbql_bind_double(odb_stmt,  9, dble(record%amdar_temperature))
        call odbql_bind_double(odb_stmt, 10, dble(record%amdar_wind_u))
        call odbql_bind_double(odb_stmt, 11, dble(record%amdar_wind_v))
        call odbql_bind_double(odb_stmt, 12, dble(record%amdar_turbulence_index))
        call odbql_bind_double(odb_stmt, 13, dble(record%amdar_specific_humidity))
        call odbql_step(odb_stmt)
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
