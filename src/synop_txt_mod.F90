module synop_txt_mod

  use datetime
  use string
  use container
  use flogger
  use params_mod
  use data_translators_utils_mod
  use synop_mod

  implicit none

  private

  public synop_txt_read

contains

  subroutine synop_txt_read(file_path, stations, records)

    character(*), intent(in) :: file_path
    type(hash_table_type), intent(inout), target :: stations
    type(linked_list_type), intent(inout), target :: records

    type(datetime_type) time
    type(synop_station_type), pointer :: station
    type(synop_record_type), pointer :: record
    character(10) platform_id
    real ta, p, wd, ws, r01, lon, lat, z
    integer iostat

    call load_cma_synop_stations(stations)

    call log_notice('Reading ' // trim(file_path) // ' ...')

    time = create_datetime(basename(file_path, ext='.txt'), '%Y%m%d%H')

    open(10, file=file_path, status='old')
    do while (.true.)
      read(10, *, iostat=iostat) platform_id, ta, p, wd, ws, r01
      ta  = merge(real_missing_value, ta , is_missing(ta , src='txt'))
      p   = merge(real_missing_value, p  , is_missing(p  , src='txt'))
      wd  = merge(real_missing_value, wd , is_missing(wd , src='txt'))
      ws  = merge(real_missing_value, ws , is_missing(ws , src='txt'))
      r01 = merge(real_missing_value, r01, is_missing(r01, src='txt'))
      ! Create station and record.
      if (stations%hashed(platform_id)) then
        select type (value => stations%value(platform_id))
        type is (synop_station_type)
          station => value
        end select
      else
        allocate(station)
        call station%init(platform_id, lon, lat, z)
        call stations%insert(platform_id, station)
      end if
      allocate(record)
      record%seq_id = records%size
      record%station => station
      record%time = time
      ! Set record.
      record%p  = p
      record%ta = ta
      record%wd = wind_direction_code(wd)
      record%ws = ws
      record%ua = wind_u_component(ws, wd)
      record%va = wind_v_component(ws, wd)
      record%r01h = r01
      call records%insert(platform_id // '@' // time%isoformat(), record)
      call station%records%insert(trim(to_str(record%seq_id)), record, nodup=.true.)
      if (iostat /= 0) exit
    end do
    close(10)

    call log_notice('Station size is ' // trim(to_str(stations%size)) // ', record size is ' // trim(to_str(records%size)) // '.')

  end subroutine synop_txt_read

end module synop_txt_mod
