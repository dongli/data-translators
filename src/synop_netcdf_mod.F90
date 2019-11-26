module synop_netcdf_mod

  use netcdf
  use synop_mod
  use utils_mod

  implicit none

  private

  public synop_netcdf_write

contains

  subroutine synop_netcdf_write(file_path, stations, records)

    character(*), intent(inout) :: file_path
    type(hash_table_type), intent(in) :: stations
    type(linked_list_type), intent(in) :: records

    type(hash_table_iterator_type) station_iterator
    type(linked_list_iterator_type) record_iterator
    integer ncid, ierr
    integer platform_dimid
    integer name_maxlen_dimid
    integer max_record_per_platform_dimid
    integer record_dimid
    integer time_varid
    integer platform_id_varid
    integer platform_idx_varid
    integer record_idx_varid
    integer lon_varid
    integer lat_varid
    integer alt_varid
    integer p_varid
    integer T_varid
    integer Td_varid
    integer rh_varid
    integer wd_varid
    integer ws_varid
    integer r01_varid

    integer, parameter :: max_record_per_platform = 20

    real(8), allocatable :: time(:)
    character(8), allocatable :: platform_id(:)
    integer, allocatable :: platform_idx(:)
    integer, allocatable :: record_idx(:,:)
    real, allocatable :: lon(:)
    real, allocatable :: lat(:)
    real, allocatable :: alt(:)
    real, allocatable :: p(:)
    real, allocatable :: T(:)
    real, allocatable :: Td(:)
    real, allocatable :: rh(:)
    real, allocatable :: wd(:)
    real, allocatable :: ws(:)
    real, allocatable :: r01(:)
    integer i, j

    if (file_path == '') file_path ='synop.nc'

    write(*, *) '[Notice]: Writing ' // trim(file_path) // ' ...'

    ierr = nf90_create(file_path, nf90_clobber, ncid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_dim(ncid, 'platform', stations%size, platform_dimid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_dim(ncid, 'name_maxlen', 8, name_maxlen_dimid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_dim(ncid, 'max_record_per_platform', max_record_per_platform, max_record_per_platform_dimid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_dim(ncid, 'record', records%size, record_dimid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'time', nf90_double, [record_dimid], time_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, time_varid, 'units', 'hours since 1970-01-01T00:00:00')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'platform_id', nf90_char, [name_maxlen_dimid, platform_dimid], platform_id_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'platform_idx', nf90_int, [record_dimid], platform_idx_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'record_idx', nf90_int, [max_record_per_platform_dimid,platform_dimid], record_idx_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'lon', nf90_float, [platform_dimid], lon_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, lon_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, lon_varid, 'units', 'degree_east')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'lat', nf90_float, [platform_dimid], lat_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, lat_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, lat_varid, 'units', 'degree_north')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'alt', nf90_float, [platform_dimid], alt_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, alt_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, alt_varid, 'units', 'm')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'p', nf90_float, [record_dimid], p_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, p_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, p_varid, 'units', 'Pa')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'ta', nf90_float, [record_dimid], T_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, T_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, T_varid, 'units', 'K')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'td', nf90_float, [record_dimid], Td_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, Td_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, Td_varid, 'units', 'K')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'rh', nf90_float, [record_dimid], rh_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, rh_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, rh_varid, 'units', '%')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'wd', nf90_float, [record_dimid], wd_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, wd_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, wd_varid, 'units', 'degree')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'ws', nf90_float, [record_dimid], ws_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, ws_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, ws_varid, 'units', 'm s-1')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'r01h', nf90_float, [record_dimid], r01_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, r01_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, r01_varid, 'units', 'mm')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_enddef(ncid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    allocate(time(records%size))
    allocate(platform_id(stations%size))
    allocate(platform_idx(records%size))
    allocate(record_idx(max_record_per_platform,stations%size))
    allocate(lon(stations%size))
    allocate(lat(stations%size))
    allocate(alt(stations%size))
    allocate(p(records%size))
    allocate(T(records%size))
    allocate(Td(records%size))
    allocate(rh(records%size))
    allocate(wd(records%size))
    allocate(ws(records%size))
    allocate(r01(records%size))

    record_idx = -1

    i = 1
    station_iterator = hash_table_iterator(stations)
    do while (.not. station_iterator%ended())
      select type (station => station_iterator%value)
      type is (synop_station_type)
        platform_id(i) = station%name
        lon(i)  = station%lon
        lat(i)  = station%lat
        alt(i)  = station%z
      end select
      i = i + 1
      call station_iterator%next()
    end do

    i = 1
    record_iterator = linked_list_iterator(records)
    do while (.not. record_iterator%ended())
      select type (record => record_iterator%value)
      type is (synop_record_type)
        time(i) = record%time%timestamp() / 3600.0
        platform_idx(i) = record%station%seq_id
        j = count(record_idx(:,platform_idx(i)+1) >= 0) + 1
        if (j > max_record_per_platform) then
          write(*, *) '[Error]: Exceeds max_record_per_platform ' // trim(to_string(max_record_per_platform)) // '!'
          stop 1
        end if
        record_idx(j,platform_idx(i)+1) = record%seq_id
        p  (i) = record%p
        T  (i) = record%ta
        Td (i) = record%td
        rh (i) = record%rh
        wd (i) = record%wd
        ws (i) = record%ws
        r01(i) = record%r01h
      end select
      i = i + 1
      call record_iterator%next()
    end do

    ierr = nf90_put_var(ncid, time_varid, time)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, platform_id_varid, platform_id)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, platform_idx_varid, platform_idx)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, record_idx_varid, record_idx)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, lon_varid, lon)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, lat_varid, lat)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, alt_varid, alt)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, p_varid, p)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, T_varid, T)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, Td_varid, Td)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, rh_varid, rh)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, wd_varid, wd)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, ws_varid, ws)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, r01_varid, r01)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_close(ncid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    deallocate(time)
    deallocate(platform_id)
    deallocate(platform_idx)
    deallocate(record_idx)
    deallocate(lon)
    deallocate(lat)
    deallocate(alt)
    deallocate(p)
    deallocate(T)
    deallocate(Td)
    deallocate(rh)
    deallocate(wd)
    deallocate(ws)
    deallocate(r01)

  end subroutine synop_netcdf_write

end module synop_netcdf_mod
