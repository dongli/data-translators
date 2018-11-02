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
      'flight_name STRING, lon REAL, lat REAL, z REAL, date STRING, time STRING, ' // &
      'amdar_pressure REAL, ' // &
      'amdar_temperature REAL, ' // &
      'amdar_wind_speed REAL, ' // &
      'amdar_wind_direction REAL, ' // &
      'amdar_specific_humidity REAL) ON "' // &
      trim(file_path) // '";', -1, odb_stmt, odb_unparsed_sql)
    call odbql_prepare_v2(odb_db, 'INSERT INTO amdar (' // &
      'flight_name, lon, lat, z, date, time, ' // &
      'amdar_pressure, ' // &
      'amdar_temperature, ' // &
      'amdar_wind_speed, ' // &
      'amdar_wind_direction, ' // &
      'amdar_specific_humidity' // &
      ') VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);', -1, odb_stmt, odb_unparsed_sql)

    record_iterator = linked_list_iterator(records)
    do while (.not. record_iterator%ended())
      select type (record => record_iterator%value)
      type is (amdar_record_type)
        call odbql_bind_text  (odb_stmt,  1, record%flight%name, len_trim(record%flight%name))
        call odbql_bind_double(odb_stmt,  2, dble(record%lon))
        call odbql_bind_double(odb_stmt,  3, dble(record%lat))
        call odbql_bind_double(odb_stmt,  4, dble(record%z))
        str = record%time%format('%Y%m%d')
        call odbql_bind_text  (odb_stmt,  5, trim(str), len_trim(str))
        str = record%time%format('%H%M%S')
        call odbql_bind_text  (odb_stmt,  6, trim(str), len_trim(str))
        call odbql_bind_double(odb_stmt,  7, dble(record%amdar_pressure))
        call odbql_bind_double(odb_stmt,  8, dble(record%amdar_temperature))
        call odbql_bind_double(odb_stmt,  9, dble(record%amdar_wind_speed))
        call odbql_bind_double(odb_stmt, 10, dble(record%amdar_wind_direction))
        call odbql_bind_double(odb_stmt, 11, dble(record%amdar_specific_humidity))
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
