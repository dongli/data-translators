program decode_prepbufr_synop

  use cli_mod
  use synop_mod
  use datetime_mod
  use timedelta_mod
  use hash_table_mod
  use linked_list_mod
  use params_mod
  use utils_mod
  use odbql_wrappers

  implicit none

  integer, parameter :: max_num_var = 35
  integer, parameter :: max_num_lev = 250
  integer, parameter :: max_num_event = 10

  type(hash_table_type) stations
  type(linked_list_type) records

  type(linked_list_iterator_type) record_iterator

  call decode(cli_get_file_path(), stations, records)
  call write_odb(stations, records)

contains

  subroutine decode(file_path, stations, records)

    character(*), intent(in) :: file_path
    type(hash_table_type), intent(out) :: stations
    type(linked_list_type), intent(out) :: records

    character(8) subset, station_name
    integer idate, iret, i
    character(10) sdate
    integer msg_count, subset_count
    real(8) hdr(max_num_var)
    real(8) obs(max_num_var,max_num_lev,max_num_event)
    real(8) qc(max_num_var,max_num_lev,max_num_event)
    real(8) pc(max_num_var,max_num_lev,max_num_event)
    real u, v
    type(datetime_type) time
    logical new_record
    type(synop_station_type), pointer :: station
    type(synop_record_type), pointer :: record

    ! BUFRLIB functions
    integer ireadmg, ireadsb

    stations = hash_table(chunk_size=50000, max_load_factor=0.9)
    call records%clear()

    open(10, file=file_path, action='read', form='unformatted')
    call openbf(10, 'IN', 10)
    call datelen(10) ! This call causes idate to be in format YYYYMMDDHH.
    do while (ireadmg(10, subset, idate) == 0) ! ireadmg returns mnemonic in subset, and copies message into internal arrays.
      msg_count = msg_count + 1
      if (subset /= 'ADPSFC') cycle
      write(sdate, "(I10)") idate
      time = datetime(sdate, '%Y%m%d%H')
      write(*, "('=> ', I5.5, X, A8)") msg_count, subset
      do while (ireadsb(10) == 0) ! ireadsb copies one subset into internal arrays.
        ! Call values-level subrountines to retrieve actual data values from this subset.
        call ufbint(10, hdr, max_num_var, 1,                          iret, 'SID DHR XOB YOB ELV TYP') ! The forth argument 1 means there is only one repetition of the mnemonics.
        call ufbevn(10, obs, max_num_var, max_num_lev, max_num_event, iret, 'POB TOB TDO UOB VOB TP01 TP03 TP06 TP12 TP24')
        call ufbevn(10, qc,  max_num_var, max_num_lev, max_num_event, iret, 'PQM TQM NUL WQM')
        call ufbevn(10, pc,  max_num_var, max_num_lev, max_num_event, iret, 'PPC TPC NUL WPC')
        station_name = transfer(hdr(1), station_name)
        time = time + timedelta(hours=int(hdr(2)))
        if (stations%hashed(station_name)) then
          select type (value => stations%value(station_name))
          type is (synop_station_type)
            station => value
          end select
        else
          allocate(station)
          station%name = station_name
          station%lon = hdr(3)
          if (station%lon > 180) station%lon = station%lon - 360
          station%lat = hdr(4)
          station%z = hdr(5)
          call stations%insert(station_name, station)
        end if
        nullify(record)
        select type (value => records%last_value())
        type is (synop_record_type)
          ! Since recode may be split into two subsets, we need to check if previous record exists with the same time.
          record => value
          if (record%station%name == station_name .and. record%time == time) then
            new_record = .false.
          else
            nullify(record)
          end if
        end select
        if (.not. associated(record)) then
          allocate(record)
          record%station => station
          record%time = time
          new_record = .true.
        end if

        if (record%sfc_pressure    == real_missing_value) record%sfc_pressure     = prepbufr_raw(obs(1,1,:), pc(1,1,:))
        if (record%sfc_temperature == real_missing_value) record%sfc_temperature  = prepbufr_raw(obs(2,1,:), pc(2,1,:))
        if (record%sfc_dewpoint    == real_missing_value) record%sfc_dewpoint     = prepbufr_raw(obs(3,1,:))
        if (record%sfc_wind_speed  == real_missing_value) then
          u = prepbufr_raw(obs(4,1,:), pc(4,1,:))
          v = prepbufr_raw(obs(5,1,:), pc(4,1,:))
          record%sfc_wind_speed     = merge(real_missing_value, sqrt(u**2 + v**2), u == real_missing_value)
          record%sfc_wind_direction = merge(real_missing_value, wind_direction(u, v), u == real_missing_value)
        end if
        if (record%sfc_rain_01h == real_missing_value) record%sfc_rain_01h = prepbufr_raw(obs(6,1,:))
        if (record%sfc_rain_03h == real_missing_value) record%sfc_rain_03h = prepbufr_raw(obs(7,1,:))
        if (record%sfc_rain_06h == real_missing_value) record%sfc_rain_06h = prepbufr_raw(obs(8,1,:))
        if (record%sfc_rain_12h == real_missing_value) record%sfc_rain_12h = prepbufr_raw(obs(9,1,:))
        if (record%sfc_rain_24h == real_missing_value) record%sfc_rain_24h = prepbufr_raw(obs(10,1,:))

        if (new_record) then
          call records%insert(station_name // '@' // time%isoformat(), record)
        end if
      end do
    end do
    call closbf(10)

  end subroutine decode

  subroutine write_odb(stations, records)

    type(hash_table_type), intent(in) :: stations
    type(linked_list_type), intent(in) :: records

    ! ODB variables
    type(odbql) odb_db
    type(odbql_stmt) odb_stmt
    type(odbql_value) odb_value
    character(100) odb_unparsed_sql
    character(50) odb_file_name

    character(30) time_str
    type(linked_list_iterator_type) record_iterator

    odb_file_name = 'synop.odb'

    ! Write ODB file.
    call odbql_open('', odb_db)
    call odbql_prepare_v2(odb_db, 'CREATE TABLE synop AS (' // &
      'station_name STRING, lon REAL, lat REAL, z REAL, time STRING, ' // &
      'sfc_temperature REAL, ' // &
      'sfc_dewpoint REAL, ' // &
      'sfc_wind_speed REAL, ' // &
      'sfc_wind_direction REAL, ' // &
      'sfc_pressure REAL) ON "' // &
      trim(odb_file_name) // '";', -1, odb_stmt, odb_unparsed_sql)
    call odbql_prepare_v2(odb_db, 'INSERT INTO synop (' // &
      'station_name, lon, lat, z, time, ' // &
      'sfc_temperature, ' // &
      'sfc_dewpoint, ' // &
      'sfc_wind_speed, ' // &
      'sfc_wind_direction, ' // &
      'sfc_pressure' // &
      ') VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?);', -1, odb_stmt, odb_unparsed_sql)

    record_iterator = linked_list_iterator(records)
    do while (.not. record_iterator%ended())
      select type (record => record_iterator%value)
      type is (synop_record_type)
        call odbql_bind_text  (odb_stmt,  1, record%station%name, len_trim(record%station%name))
        call odbql_bind_double(odb_stmt,  2, dble(record%station%lon))
        call odbql_bind_double(odb_stmt,  3, dble(record%station%lat))
        call odbql_bind_double(odb_stmt,  4, dble(record%station%z))
        time_str = record%time%format('%Y%m%d%H%M')
        time_str = 'N/A'
        call odbql_bind_text  (odb_stmt,  5, time_str, len_trim(time_str))
        call odbql_bind_double(odb_stmt,  6, dble(record%sfc_temperature))
        call odbql_bind_double(odb_stmt,  7, dble(record%sfc_dewpoint))
        call odbql_bind_double(odb_stmt,  8, dble(record%sfc_wind_speed))
        call odbql_bind_double(odb_stmt,  9, dble(record%sfc_wind_direction))
        call odbql_bind_double(odb_stmt, 10, dble(record%sfc_pressure))
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

  end subroutine write_odb

end program decode_prepbufr_synop
