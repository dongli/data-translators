module metar_odb_mod

  use hash_table_mod
  use linked_list_mod
  use metar_mod
  use odbql_wrappers

  private

  public metar_odb_write

contains

  subroutine metar_odb_write()

    ! ODB variables
    type(odbql) odb_db
    type(odbql_stmt) odb_stmt
    type(odbql_value) odb_value
    character(100) odb_unparsed_sql
    character(50) odb_file_name

    character(30) str
    type(linked_list_iterator_type) record_iterator

    odb_file_name = 'metar.odb'

    ! Write ODB file.
    call odbql_open('', odb_db)
    call odbql_prepare_v2(odb_db, 'CREATE TABLE metar AS (' // &
      'station_name STRING, lon REAL, lat REAL, z REAL, date STRING, time STRING, ' // &
      'sfc_temperature REAL, ' // &
      'sfc_dewpoint REAL, ' // &
      'sfc_wind_speed REAL, ' // &
      'sfc_wind_direction REAL, ' // &
      'sfc_pressure REAL) ON "' // &
      trim(odb_file_name) // '";', -1, odb_stmt, odb_unparsed_sql)
    call odbql_prepare_v2(odb_db, 'INSERT INTO metar (' // &
      'station_name, lon, lat, z, date, time, ' // &
      'sfc_temperature, ' // &
      'sfc_dewpoint, ' // &
      'sfc_wind_speed, ' // &
      'sfc_wind_direction, ' // &
      'sfc_pressure' // &
      ') VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);', -1, odb_stmt, odb_unparsed_sql)

    record_iterator = linked_list_iterator(records)
    do while (.not. record_iterator%ended())
      select type (record => record_iterator%value)
      type is (metar_record_type)
        call odbql_bind_text  (odb_stmt,  1, record%station%name, len_trim(record%station%name))
        call odbql_bind_double(odb_stmt,  2, dble(record%station%lon))
        call odbql_bind_double(odb_stmt,  3, dble(record%station%lat))
        call odbql_bind_double(odb_stmt,  4, dble(record%station%z))
        str = record%time%format('%Y%m%d')
        call odbql_bind_text  (odb_stmt,  5, trim(str), len_trim(str))
        str = record%time%format('%H%M%S')
        call odbql_bind_text  (odb_stmt,  6, trim(str), len_trim(str))
        call odbql_bind_double(odb_stmt,  7, dble(record%sfc_temperature))
        call odbql_bind_double(odb_stmt,  8, dble(record%sfc_dewpoint))
        call odbql_bind_double(odb_stmt,  9, dble(record%sfc_wind_speed))
        call odbql_bind_double(odb_stmt, 10, dble(record%sfc_wind_direction))
        call odbql_bind_double(odb_stmt, 11, dble(record%sfc_pressure))
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

  end subroutine metar_odb_write

end module metar_odb_mod