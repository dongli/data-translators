module synop_odb_mod

  use hash_table_mod
  use linked_list_mod
  use synop_mod
  use odbql_wrappers

  private

  public synop_odb_write

contains

  subroutine synop_odb_write(file_path)

    character(*), intent(inout) :: file_path

    ! ODB variables
    type(odbql) odb_db
    type(odbql_stmt) odb_stmt
    type(odbql_value) odb_value
    character(100) :: odb_unparsed_sql = ''

    character(30) str
    type(linked_list_iterator_type) record_iterator

    if (file_path == '') file_path ='synop.odb'

    ! Write ODB file.
    call odbql_open('', odb_db)
    call odbql_prepare_v2(odb_db, 'CREATE TABLE synop AS (' // &
      'station_name STRING, ' // &              !  1
      'lon REAL, ' // &                         !  2
      'lat REAL, ' // &                         !  3
      'z REAL, ' // &                           !  4
      'date STRING, ' // &                      !  5
      'time STRING, ' // &                      !  6
      'type INTEGER, ' // &                     !  7
      'temperature REAL, ' // &                 !  8
      'temperature_qc INTEGER, ' // &           !  9
      'dewpoint REAL, ' // &                    ! 10
      'wind_u REAL, ' // &                      ! 11
      'wind_v REAL, ' // &                      ! 12
      'wind_qc INTEGER, ' // &                  ! 13
      'pressure REAL, ' // &                    ! 14
      'pressure_qc INTEGER, ' // &              ! 15
      ') ON "' // trim(file_path) // '";', -1, odb_stmt, odb_unparsed_sql)
    call odbql_prepare_v2(odb_db, 'INSERT INTO synop (' // &
      'station_name, ' // &                     !  1
      'lon, ' // &                              !  2
      'lat, ' // &                              !  3
      'z, ' // &                                !  4
      'date, ' // &                             !  5
      'time, ' // &                             !  6
      'type, ' // &                             !  7
      'temperature, ' // &                      !  8
      'temperature_qc, ' // &                   !  9
      'dewpoint, ' // &                         ! 10
      'wind_u, ' // &                           ! 11
      'wind_v, ' // &                           ! 12
      'wind_qc, ' // &                          ! 13
      'pressure' // &                           ! 14
      'pressure_qc' // &                        ! 15
      ') VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);', -1, odb_stmt, odb_unparsed_sql)

    record_iterator = linked_list_iterator(records)
    do while (.not. record_iterator%ended())
      select type (record => record_iterator%value)
      type is (synop_record_type)
        call odbql_bind_text  (odb_stmt,  1, record%station%name, len_trim(record%station%name))
        call odbql_bind_double(odb_stmt,  2, dble(record%station%lon))
        call odbql_bind_double(odb_stmt,  3, dble(record%station%lat))
        call odbql_bind_double(odb_stmt,  4, dble(record%station%z))
        str = record%time%format('%Y%m%d')
        call odbql_bind_text  (odb_stmt,  5, trim(str), len_trim(str))
        str = record%time%format('%H%M%S')
        call odbql_bind_text  (odb_stmt,  6, trim(str), len_trim(str))
        call odbql_bind_int   (odb_stmt,  7, record%type)
        call odbql_bind_double(odb_stmt,  8, dble(record%sfc_temperature))
        call odbql_bind_int   (odb_stmt,  9, record%sfc_temperature_qc)
        call odbql_bind_double(odb_stmt, 10, dble(record%sfc_dewpoint))
        call odbql_bind_double(odb_stmt, 11, dble(record%sfc_wind_u))
        call odbql_bind_double(odb_stmt, 12, dble(record%sfc_wind_v))
        call odbql_bind_int   (odb_stmt, 13, record%sfc_wind_qc)
        call odbql_bind_double(odb_stmt, 14, dble(record%sfc_pressure))
        call odbql_bind_int   (odb_stmt, 15, record%sfc_pressure_qc)
        call odbql_step(odb_stmt)
      class default
        write(*, *) '[Error]: Unknown record in the list!'
        stop 1
      end select
      call record_iterator%next()
    end do

    call odbql_finalize(odb_stmt)
    call odbql_close(odb_db)

  end subroutine synop_odb_write

end module synop_odb_mod
