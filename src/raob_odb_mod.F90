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

    integer i
    character(30) str
    type(linked_list_iterator_type) record_iterator

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
      'wind_u REAL, ' // &                        ! 11
      'wind_v REAL, ' // &                        ! 12
      'specific_humidity REAL, ' // &             ! 13
      'relative_humidity REAL' // &               ! 14
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
      'wind_u, ' // &                             ! 11
      'wind_v, ' // &                             ! 12
      'specific_humidity,' // &                   ! 13
      'relative_humidity' // &                    ! 14
      ') VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);', -1, odb_stmt, odb_unparsed_sql)

    record_iterator = linked_list_iterator(records)
    do while (.not. record_iterator%ended())
      select type (record => record_iterator%value)
      type is (raob_record_type)
        ! Mandatory levels
        do i = 1, record%snd_man%num_level
          call odbql_bind_text  (odb_stmt,    1, record%station%name, len_trim(record%station%name))
          call odbql_bind_double(odb_stmt,    2, dble(record%station%lon))
          call odbql_bind_double(odb_stmt,    3, dble(record%station%lat))
          call odbql_bind_double(odb_stmt,    4, dble(record%station%z))
          str = record%time%format('%Y%m%d')
          call odbql_bind_text  (odb_stmt,    5, trim(str), len_trim(str))
          str = record%time%format('%H%M%S')
          call odbql_bind_text  (odb_stmt,    6, trim(str), len_trim(str))
          call odbql_bind_double(odb_stmt,    7, dble(record%snd_man%height(i)))
          call odbql_bind_double(odb_stmt,    8, dble(record%snd_man%pressure(i)))
          call odbql_bind_double(odb_stmt,    9, dble(record%snd_man%temperature(i)))
          call odbql_bind_double(odb_stmt,   10, dble(record%snd_man%dewpoint(i)))
          call odbql_bind_double(odb_stmt,   11, dble(record%snd_man%wind_u(i)))
          call odbql_bind_double(odb_stmt,   12, dble(record%snd_man%wind_v(i)))
          call odbql_bind_double(odb_stmt,   13, dble(record%snd_man%specific_humidity(i)))
          call odbql_bind_double(odb_stmt,   14, dble(record%snd_man%relative_humidity(i)))
          call odbql_step(odb_stmt)
        end do
        ! Significant levels
        do i = 1, record%snd_sigt%num_level
          call odbql_bind_text  (odb_stmt,    1, record%station%name, len_trim(record%station%name))
          call odbql_bind_double(odb_stmt,    2, dble(record%station%lon))
          call odbql_bind_double(odb_stmt,    3, dble(record%station%lat))
          call odbql_bind_double(odb_stmt,    4, dble(record%station%z))
          str = record%time%format('%Y%m%d')
          call odbql_bind_text  (odb_stmt,    5, trim(str), len_trim(str))
          str = record%time%format('%H%M%S')
          call odbql_bind_text  (odb_stmt,    6, trim(str), len_trim(str))
          call odbql_bind_double(odb_stmt,    7, dble(record%snd_sigt%height(i)))
          call odbql_bind_double(odb_stmt,    8, dble(record%snd_sigt%pressure(i)))
          call odbql_bind_double(odb_stmt,    9, dble(record%snd_sigt%temperature(i)))
          call odbql_bind_double(odb_stmt,   10, dble(record%snd_sigt%dewpoint(i)))
          call odbql_bind_double(odb_stmt,   11, dble(record%snd_sigt%wind_u(i)))
          call odbql_bind_double(odb_stmt,   12, dble(record%snd_sigt%wind_v(i)))
          call odbql_bind_double(odb_stmt,   13, dble(record%snd_sigt%specific_humidity(i)))
          call odbql_bind_double(odb_stmt,   14, dble(record%snd_sigt%relative_humidity(i)))
          call odbql_step(odb_stmt)
        end do
        ! Wind levels
        do i = 1, record%snd_sigt%num_level
          call odbql_bind_text  (odb_stmt,    1, record%station%name, len_trim(record%station%name))
          call odbql_bind_double(odb_stmt,    2, dble(record%station%lon))
          call odbql_bind_double(odb_stmt,    3, dble(record%station%lat))
          call odbql_bind_double(odb_stmt,    4, dble(record%station%z))
          str = record%time%format('%Y%m%d')
          call odbql_bind_text  (odb_stmt,    5, trim(str), len_trim(str))
          str = record%time%format('%H%M%S')
          call odbql_bind_text  (odb_stmt,    6, trim(str), len_trim(str))
          call odbql_bind_double(odb_stmt,    7, dble(record%snd_sigw%height(i)))
          call odbql_bind_double(odb_stmt,    8, dble(record%snd_sigw%pressure(i)))
          call odbql_bind_double(odb_stmt,    9, dble(real_missing_value))
          call odbql_bind_double(odb_stmt,   10, dble(real_missing_value))
          call odbql_bind_double(odb_stmt,   11, dble(record%snd_sigw%wind_u(i)))
          call odbql_bind_double(odb_stmt,   12, dble(record%snd_sigw%wind_v(i)))
          call odbql_step(odb_stmt)
        end do
        ! Tropopause levels
        do i = 1, record%snd_sigt%num_level
          call odbql_bind_text  (odb_stmt,    1, record%station%name, len_trim(record%station%name))
          call odbql_bind_double(odb_stmt,    2, dble(record%station%lon))
          call odbql_bind_double(odb_stmt,    3, dble(record%station%lat))
          call odbql_bind_double(odb_stmt,    4, dble(record%station%z))
          str = record%time%format('%Y%m%d')
          call odbql_bind_text  (odb_stmt,    5, trim(str), len_trim(str))
          str = record%time%format('%H%M%S')
          call odbql_bind_text  (odb_stmt,    6, trim(str), len_trim(str))
          call odbql_bind_double(odb_stmt,    7, dble(record%snd_trop%height(i)))
          call odbql_bind_double(odb_stmt,    8, dble(record%snd_trop%pressure(i)))
          call odbql_bind_double(odb_stmt,    9, dble(record%snd_trop%temperature(i)))
          call odbql_bind_double(odb_stmt,   10, dble(record%snd_trop%dewpoint(i)))
          call odbql_bind_double(odb_stmt,   11, dble(record%snd_trop%wind_u(i)))
          call odbql_bind_double(odb_stmt,   12, dble(record%snd_trop%wind_v(i)))
          call odbql_bind_double(odb_stmt,   13, dble(record%snd_trop%specific_humidity(i)))
          call odbql_bind_double(odb_stmt,   14, dble(record%snd_trop%relative_humidity(i)))
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

  end subroutine raob_odb_write

end module raob_odb_mod
