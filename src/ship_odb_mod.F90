module ship_odb_mod

  use hash_table_mod
  use linked_list_mod
  use ship_mod
  use odbql_wrappers

  implicit none

  private

  public ship_odb_write

contains


  subroutine ship_odb_write(file_path)

    character(*), intent(inout) :: file_path

    ! ODB variables
    type(odbql) odb_db
    type(odbql_stmt) odb_stmt
    type(odbql_value) odb_value
    character(100) odb_unparsed_sql

    character(30) str
    type(linked_list_iterator_type) record_iterator

    if (file_path == '') file_path = 'ship.odb'

    ! Write ODB file.
    call odbql_open('', odb_db)
    call odbql_prepare_v2(odb_db, 'CREATE TABLE ship AS (' // &
      'ship_name STRING, lon REAL, lat REAL, date STRING, time STRING, ' // &
      'ship_pressure REAL, ' // &
      'ship_air_temperature REAL, ' // &
      'ship_sea_temperature REAL, ' // &
      'ship_dewpoint REAL, ' // &
      'ship_relative_humidity REAL, ' // &
      'ship_specific_humidity REAL, ' // &
      'ship_wind_speed REAL, ' // &
      'ship_wind_direction REAL ' // &
      ') ON "' // trim(file_path) // '";', -1, odb_stmt, odb_unparsed_sql)
    call odbql_prepare_v2(odb_db, 'INSERT INTO ship (' // &
      'ship_name, lon, lat, date, time, ' // &
      'ship_pressure, ' // &
      'ship_air_temperature, ' // &
      'ship_sea_temperature, ' // &
      'ship_dewpoint, ' // &
      'ship_relative_humidity, ' // &
      'ship_specific_humidity, ' // &
      'ship_wind_speed, ' // &
      'ship_wind_direction ' // &
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
        call odbql_bind_double(odb_stmt, 12, dble(record%ship_wind_speed))
        call odbql_bind_double(odb_stmt, 13, dble(record%ship_wind_direction))
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
