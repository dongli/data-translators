module anem_txt_mod

  use anem_nrg_mod
  use datetime
  use string
  use container
  use flogger
  use regex
  use params_mod
  use utils_mod
  use cli_mod

  implicit none

  private

  public anem_txt_read

  character(5), allocatable :: tower_names(:)
  real, allocatable :: tower_lons(:)
  real, allocatable :: tower_lats(:)
  real, allocatable :: tower_alts(:)
  real, allocatable :: tower_w_hgt(:,:)

contains

  subroutine anem_txt_read(file_path, towers, records)

    character(*), intent(in) :: file_path
    type(hash_table_type), intent(inout), target :: towers
    type(linked_list_type), intent(inout), target :: records

    type(anem_nrg_tower_type), pointer :: tower
    type(anem_nrg_record_type), pointer :: record
    type(hash_table_type) local_records
    type(hash_table_iterator_type) record_iterator
    type(datetime_type) time

    character(30) tower_name, time_str
    integer fu, istat, i
    real lon, lat, alt
    real, allocatable :: w_hgt(:)
    real hgt, wd, ws, ws_min, ws_max, ws_std, ta, p, rh

    call read_tower_info()

    local_records = hash_table(chunk_size=100000, max_load_factor=0.9)

    call log_notice('Reading ' // trim(file_path) // ' ...')
    fu = unique_file_number()
    open(fu, file=file_path, status='old')
    do while (.true.)
      read(fu, *, iostat=istat) tower_name, hgt, time_str, wd, ws, ws_min, ws_max, ws_std, ta, p, rh
      if (istat < 0) exit
      ! Create tower object if needed.
      if (towers%hashed(tower_name)) then
        select type (value => towers%value(tower_name))
        type is (anem_nrg_tower_type)
          tower => value
        end select
      else
        allocate(tower)
        call query_tower_info(tower_name, lon, lat, alt, w_hgt)
        call tower%init(tower_name, lon, lat, alt)
        tower%seq_id = towers%size
        call towers%insert(tower_name, tower)
      end if
      ! Create record object if needed.
      if (wd /= 99999.0 .and. ws /= 99999.0) then
        time = create_datetime(time_str(1:19), '%Y-%m-%dT%H:%M:%S')
        record => get_record(local_records, time, tower, size(w_hgt))
        record%h = w_hgt
        do i = 1, size(w_hgt)
          if (hgt == w_hgt(i)) exit
        end do
        record%wd_avg(i) = wd
        record%ws_avg(i) = ws
        record%ws_min(i) = ws_min
        record%ws_max(i) = ws_max
        record%ws_std(i) = ws_std
        record%ua_avg(i) = wind_u_component(ws, wd)
        record%va_avg(i) = wind_v_component(ws, wd)
      end if
    end do
    close(fu)

    record_iterator = hash_table_iterator(local_records)
    do while (.not. record_iterator%ended())
      select type (record => record_iterator%value)
      type is (anem_nrg_record_type)
        if (cli_start_time%year == 0 .or. (cli_start_time <= record%time .and. record%time <= cli_end_time)) then
          call records%insert(record)
        end if
      end select
      call record_iterator%next()
    end do

    call log_notice('Tower size is ' // trim(to_string(towers%size)) // ', record size is ' // trim(to_string(records%size)) // '.')

    call free_tower_arrays()

  end subroutine anem_txt_read

  subroutine read_tower_info()

    use netcdf

    integer ncid, tower_dimid, max_obs_heights_dimid
    integer name_varid, lon_varid, lat_varid, alt_varid, w_hgt_varid, ierr
    integer num_tower, max_obs_heights

    ierr = NF90_OPEN('/data/home/longrun/dongli/tower_info.nc', NF90_NOWRITE, ncid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)
    ierr = NF90_INQ_DIMID(ncid, 'tower', tower_dimid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)
    ierr = NF90_INQUIRE_DIMENSION(ncid, tower_dimid, len=num_tower)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)
    ierr = NF90_INQ_DIMID(ncid, 'max_obs_heights', max_obs_heights_dimid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)
    ierr = NF90_INQUIRE_DIMENSION(ncid, max_obs_heights_dimid, len=max_obs_heights)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)
    ierr = NF90_INQ_VARID(ncid, 'name', name_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)
    ierr = NF90_INQ_VARID(ncid, 'lon', lon_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)
    ierr = NF90_INQ_VARID(ncid, 'lat', lat_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)
    ierr = NF90_INQ_VARID(ncid, 'alt', alt_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)
    ierr = NF90_INQ_VARID(ncid, 'wspd_height', w_hgt_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)
    allocate(tower_names(num_tower))
    allocate(tower_lons(num_tower))
    allocate(tower_lats(num_tower))
    allocate(tower_alts(num_tower))
    allocate(tower_w_hgt(max_obs_heights,num_tower))
    ierr = NF90_GET_VAR(ncid, name_varid, tower_names)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)
    ierr = NF90_GET_VAR(ncid, lon_varid, tower_lons)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)
    ierr = NF90_GET_VAR(ncid, lat_varid, tower_lats)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)
    ierr = NF90_GET_VAR(ncid, alt_varid, tower_alts)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)
    ierr = NF90_GET_VAR(ncid, w_hgt_varid, tower_w_hgt)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

  end subroutine read_tower_info

  subroutine free_tower_arrays()

    if (allocated(tower_names)) deallocate(tower_names)
    if (allocated(tower_lons )) deallocate(tower_lons )
    if (allocated(tower_lats )) deallocate(tower_lats )
    if (allocated(tower_alts )) deallocate(tower_alts )
    if (allocated(tower_w_hgt)) deallocate(tower_w_hgt)

  end subroutine free_tower_arrays

  subroutine query_tower_info(tower_name, lon, lat, alt, hgt)

    character(*), intent(in) :: tower_name
    real, intent(out) :: lon
    real, intent(out) :: lat
    real, intent(out) :: alt
    real, intent(out), allocatable :: hgt(:)

    character(5) s
    integer i

    write(s, '(I5.5)') to_integer(tower_name)

    do i = 1, size(tower_names)
      if (trim(tower_names(i)) == s .or. trim(tower_names(i)) == trim(tower_name)) then
        lon = tower_lons(i)
        lat = tower_lats(i)
        alt = tower_alts(i)
        hgt = tower_w_hgt(1:count(tower_w_hgt(:,i) /= 0),i)
        return
      end if
    end do
    call log_error('Failed to query tower ' // trim(tower_name) // '!')

  end subroutine query_tower_info

  function get_record(records, time, tower, num_level) result(res)

    type(hash_table_type), intent(inout) :: records
    type(datetime_type), intent(in) :: time
    type(anem_nrg_tower_type), intent(in), target :: tower
    integer, intent(in) :: num_level
    type(anem_nrg_record_type), pointer :: res

    if (.not. records%hashed(time%isoformat())) then
      allocate(res)
      res%tower => tower
      res%time = time
      call res%init(num_level)
      call records%insert(time%isoformat(), res)
    end if
    select type (value => records%value(time%isoformat()))
    type is (anem_nrg_record_type)
      res => value
    class default
      call log_error('Internal error!', __FILE__, __LINE__)
    end select

  end function get_record

end module anem_txt_mod
