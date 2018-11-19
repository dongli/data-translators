module ship_odb_mod

  use hash_table_mod
  use linked_list_mod
  use ship_mod
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
    type(odbql_value) odb_value
    character(100) odb_unparsed_sql

    character(30) str
    type(linked_list_iterator_type) record_iterator

    if (records%size == 0) then
      file_path = ''
      return
    end if

    if (file_path == '') file_path = 'ship.odb'

    ! Write ODB file.
    call odbql_open('', odb_db)
    call odbql_prepare_v2(odb_db, 'CREATE TABLE ship AS (' // &
      'ship_name STRING, ' // &                 !  1
      'lon REAL, ' // &                         !  2
      'lat REAL, ' // &                         !  3
      'date STRING, ' // &                      !  4
      'time STRING, ' // &                      !  5
      'pressure REAL, ' // &                    !  6
      'air_temperature REAL, ' // &             !  7
      'sea_temperature REAL, ' // &             !  8
      'dewpoint REAL, ' // &                    !  9
      'relative_humidity REAL, ' // &           ! 10
      'specific_humidity REAL, ' // &           ! 11
      'wind_u REAL, ' // &                      ! 12
      'wind_v REAL ' // &                       ! 13
      ') ON "' // trim(file_path) // '";', -1, odb_stmt, odb_unparsed_sql)
    call odbql_prepare_v2(odb_db, 'INSERT INTO ship (' // &
      'ship_name, ' // &                        !  1
      'lon, ' // &                              !  2
      'lat, ' // &                              !  3
      'date, ' // &                             !  4
      'time, ' // &                             !  5
      'pressure, ' // &                         !  6
      'air_temperature, ' // &                  !  7
      'sea_temperature, ' // &                  !  8
      'dewpoint, ' // &                         !  9
      'relative_humidity, ' // &                ! 10
      'specific_humidity, ' // &                ! 11
      'wind_u, ' // &                           ! 12
      'wind_v ' // &                            ! 13
      ') VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);', -1, odb_stmt, odb_unparsed_sql)

    record_iterator = linked_list_iterator(records)
    do while (.not. record_iterator%ended())
      select type (record => record_iterator%value)
      type is (ship_record_type)
        call odbql_bind_text  (odb_stmt,  1, record%ship%name, len_trim(record%ship%name))
        call odbql_bind_double(odb_stmt,  2, dble(record%lon))
        call odbql_bind_double(odb_stmt,  3, dble(record%lat))
        str = record%time%format('%Y%m%d')
        call odbql_bind_text  (odb_stmt,  4, trim(str), len_trim(str))
        str = record%time%format('%H%M%S')
        call odbql_bind_text  (odb_stmt,  5, trim(str), len_trim(str))
        call odbql_bind_double(odb_stmt,  6, dble(record%ship_pressure))
        call odbql_bind_double(odb_stmt,  7, dble(record%ship_air_temperature))
        call odbql_bind_double(odb_stmt,  8, dble(record%ship_sea_temperature))
        call odbql_bind_double(odb_stmt,  9, dble(record%ship_dewpoint))
        call odbql_bind_double(odb_stmt, 10, dble(record%ship_relative_humidity))
        call odbql_bind_double(odb_stmt, 11, dble(record%ship_specific_humidity))
        call odbql_bind_double(odb_stmt, 12, dble(record%ship_wind_u))
        call odbql_bind_double(odb_stmt, 13, dble(record%ship_wind_v))
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

  end subroutine ship_odb_write

end module ship_odb_mod
