module profiler_odb_mod

  use hash_table_mod
  use linked_list_mod
  use profiler_mod
  use odbql_wrappers

  implicit none

  private

  public profiler_odb_write

contains

  subroutine profiler_odb_write(file_path, stations, records)

    character(*), intent(inout) :: file_path
    type(hash_table_type), intent(inout) :: stations
    type(linked_list_type), intent(inout) :: records

    ! ODB variables
    type(odbql) odb_db
    type(odbql_stmt) odb_stmt
    type(odbql_value) odb_value
    character(100) odb_unparsed_sql

    integer i
    character(30) str
    type(linked_list_iterator_type) record_iterator

    if (file_path == '') file_path = 'profiler.odb'

    ! Write ODB file.
    call odbql_open('', odb_db)
    call odbql_prepare_v2(odb_db, 'CREATE TABLE profiler AS (' // &
      'station_name STRING, ' // &                !  1
      'lon REAL, ' // &                           !  2
      'lat REAL, ' // &                           !  3
      'z REAL, ' // &                             !  4
      'date STRING, ' // &                        !  5
      'time STRING, ' // &                        !  6
      'height REAL, ' // &                        !  7
      'pressure REAL, ' // &                      !  8
      'wind_u REAL, ' // &                        !  9
      'wind_v REAL' // &                          ! 10
      ') ON "' // trim(file_path) // '";', -1, odb_stmt, odb_unparsed_sql)
    call odbql_prepare_v2(odb_db, 'INSERT INTO profiler (' // &
      'station_name, ' // &                       !  1
      'lon, ' // &                                !  2
      'lat, ' // &                                !  3
      'z, ' // &                                  !  4
      'date, ' // &                               !  5
      'time, ' // &                               !  6
      'height, ' // &                             !  7
      'pressure, ' // &                           !  8
      'wind_u, ' // &                             !  9
      'wind_v' // &                               ! 10
      ') VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?);', -1, odb_stmt, odb_unparsed_sql)

    record_iterator = linked_list_iterator(records)
    do while (.not. record_iterator%ended())
      select type (record => record_iterator%value)
      type is (profiler_record_type)
        do i = 1, record%pro%num_level
          call odbql_bind_text  (odb_stmt,    1, record%station%name, len_trim(record%station%name))
          call odbql_bind_double(odb_stmt,    2, dble(record%station%lon))
          call odbql_bind_double(odb_stmt,    3, dble(record%station%lat))
          call odbql_bind_double(odb_stmt,    4, dble(record%station%z))
          str = record%time%format('%Y%m%d')
          call odbql_bind_text  (odb_stmt,    5, trim(str), len_trim(str))
          str = record%time%format('%H%M%S')
          call odbql_bind_text  (odb_stmt,    6, trim(str), len_trim(str))
          call odbql_bind_double(odb_stmt,    7, dble(record%pro%height(i)))
          call odbql_bind_double(odb_stmt,    8, dble(record%pro%pressure(i)))
          call odbql_bind_double(odb_stmt,    9, dble(record%pro%wind_u(i)))
          call odbql_bind_double(odb_stmt,   10, dble(record%pro%wind_v(i)))
          call odbql_step(odb_stmt)
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

  end subroutine profiler_odb_write

end module profiler_odb_mod
