module ship_netcdf_mod

  use netcdf
  use ship_mod
  use utils_mod

  implicit none

  private

  public ship_netcdf_write

contains

  subroutine ship_netcdf_write(file_path, ships, records)

    character(*), intent(inout) :: file_path
    type(hash_table_type), intent(in) :: ships
    type(linked_list_type), intent(in) :: records

    type(hash_table_iterator_type) ship_iterator
    type(linked_list_iterator_type) record_iterator
    integer ncid, ierr, i, j
    integer ship_dimid
    integer name_maxlen_dimid
    integer source_maxlen_dimid
    integer max_record_per_ship_dimid
    integer record_dimid
    integer time_varid
    integer ship_name_varid
    integer ship_idx_varid
    integer record_idx_varid
    integer src_varid
    integer lon_varid
    integer lat_varid
    integer p_varid
    integer Ta_varid
    integer sst_varid
    integer Td_varid
    integer rh_varid
    integer sh_varid
    integer u_varid
    integer v_varid
    integer wd_varid
    integer ws_varid
    integer wvp_varid
    integer wvh_varid
    integer svp_varid
    integer svh_varid
    integer svd_varid
    integer vis_varid
    integer cld_varid
    integer ice_varid

    integer, parameter :: max_record_per_ship = 100

    real(8), allocatable :: time(:)
    character(8), allocatable :: ship_name(:)
    integer, allocatable :: ship_idx(:)
    integer, allocatable :: record_idx(:,:)
    character(10), allocatable :: src(:)
    real, allocatable :: lon(:)
    real, allocatable :: lat(:)
    real, allocatable :: p(:)
    real, allocatable :: Ta(:)
    real, allocatable :: sst(:)
    real, allocatable :: Td(:)
    real, allocatable :: rh(:)
    real, allocatable :: sh(:)
    real, allocatable :: u(:)
    real, allocatable :: v(:)
    real, allocatable :: wd(:)
    real, allocatable :: ws(:)
    real, allocatable :: wvh(:)
    real, allocatable :: wvp(:)
    real, allocatable :: svh(:)
    real, allocatable :: svp(:)
    real, allocatable :: svd(:)
    real, allocatable :: vis(:)
    real, allocatable :: cld(:)
    real, allocatable :: ice(:)

    if (file_path == '') file_path ='ship.nc'

    write(*, *) '[Notice]: Writing ' // trim(file_path) // ' ...'

    ierr = nf90_create(file_path, nf90_clobber, ncid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_dim(ncid, 'ship', ships%size, ship_dimid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_dim(ncid, 'name_maxlen', 8, name_maxlen_dimid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_dim(ncid, 'source_maxlen', 10, source_maxlen_dimid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_dim(ncid, 'record', records%size, record_dimid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_dim(ncid, 'max_record_per_ship', max_record_per_ship, max_record_per_ship_dimid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'time', nf90_double, [record_dimid], time_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, time_varid, 'units', 'hours since 1970-01-01T00:00:00')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'ship_name', nf90_char, [name_maxlen_dimid,ship_dimid], ship_name_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'ship_idx', nf90_int, [record_dimid], ship_idx_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'record_idx', nf90_int, [max_record_per_ship_dimid,ship_dimid], record_idx_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'source', nf90_char, [source_maxlen_dimid,record_dimid], src_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, src_varid, 'long_name', 'Data source code')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'lon', nf90_float, [record_dimid], lon_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, lon_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, lon_varid, 'units', 'degree_east')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'lat', nf90_float, [record_dimid], lat_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, lat_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, lat_varid, 'units', 'degree_north')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'pressure', nf90_float, [record_dimid], p_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, p_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, p_varid, 'units', 'Pa')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'air_temperature', nf90_float, [record_dimid], Ta_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, Ta_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, Ta_varid, 'units', 'degC')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'sea_temperature', nf90_float, [record_dimid], sst_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, sst_varid, 'units', 'degC')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, sst_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'dewpoint', nf90_float, [record_dimid], Td_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, Td_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, Td_varid, 'units', 'degC')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'relative_humidity', nf90_float, [record_dimid], rh_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, rh_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, rh_varid, 'units', '%')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'specific_humidity', nf90_float, [record_dimid], sh_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, sh_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, sh_varid, 'units', 'mg kg-1')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'wind_direction', nf90_float, [record_dimid], wd_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, wd_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'wind_speed', nf90_float, [record_dimid], ws_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, ws_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'wind_wave_height', nf90_float, [record_dimid], wvh_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, wvh_varid, 'units', 'm')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, wvh_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'wind_wave_period', nf90_float, [record_dimid], wvp_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, wvp_varid, 'units', 's')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, wvp_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'surge_wave_height', nf90_float, [record_dimid], svh_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, svh_varid, 'units', 'm')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, svh_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'surge_wave_period', nf90_float, [record_dimid], svp_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, svp_varid, 'units', 's')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, svp_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'surge_wave_direction', nf90_float, [record_dimid], svd_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, svd_varid, 'units', 'deg')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, svd_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'visibility', nf90_float, [record_dimid], vis_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, vis_varid, 'units', 'm')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, vis_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'cloud_cover', nf90_float, [record_dimid], cld_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, cld_varid, 'units', '%')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, cld_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'ice_cover', nf90_float, [record_dimid], ice_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, ice_varid, 'units', '%')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, ice_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_enddef(ncid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    allocate(time(records%size))
    allocate(ship_name(ships%size))
    allocate(ship_idx(records%size))
    allocate(record_idx(max_record_per_ship,ships%size))
    allocate(src(records%size))
    allocate(lon(records%size))
    allocate(lat(records%size))
    allocate(p(records%size))
    allocate(Ta(records%size))
    allocate(sst(records%size))
    allocate(Td(records%size))
    allocate(rh(records%size))
    allocate(sh(records%size))
    allocate(wd(records%size))
    allocate(ws(records%size))
    allocate(wvh(records%size))
    allocate(wvp(records%size))
    allocate(svh(records%size))
    allocate(svp(records%size))
    allocate(svd(records%size))
    allocate(vis(records%size))
    allocate(cld(records%size))
    allocate(ice(records%size))

    record_idx = -1

    i = 1
    ship_iterator = hash_table_iterator(ships)
    do while (.not. ship_iterator%ended())
      select type (ship => ship_iterator%value)
      type is (ship_type)
        ship_name(i) = ship%name
      end select
      i = i + 1
      call ship_iterator%next()
    end do

    i = 1
    record_iterator = linked_list_iterator(records)
    do while (.not. record_iterator%ended())
      select type (record => record_iterator%value)
      type is (ship_record_type)
        time(i) = record%time%timestamp() / 3600.0
        ship_idx(i) = record%ship%seq_id
        j = count(record_idx(:,ship_idx(i)+1) >= 0) + 1
        if (j > max_record_per_ship) then
          write(*, *) '[Error]: Exceeds max_record_per_ship ' // trim(to_string(max_record_per_ship)) // '!'
          stop 1
        end if
        record_idx(j,ship_idx(i)+1) = record%seq_id
        src(i) = record%source
        lon(i) = record%lon
        lat(i) = record%lat
        p  (i) = record%pressure
        Ta (i) = record%air_temperature
        sst(i) = record%sea_temperature
        Td (i) = record%dewpoint
        rh (i) = record%relative_humidity
        sh (i) = record%specific_humidity
        wd (i) = record%wind_direction
        ws (i) = record%wind_speed
        wvh(i) = record%wind_wave_height
        wvp(i) = record%wind_wave_period
        svh(i) = record%surge_wave_height
        svp(i) = record%surge_wave_period
        svd(i) = record%surge_wave_direction
        vis(i) = record%visibility
        cld(i) = record%cloud_cover
        ice(i) = record%ice_cover
      end select
      i = i + 1
      call record_iterator%next()
    end do

    ierr = nf90_put_var(ncid, time_varid, time)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, ship_name_varid, ship_name)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, ship_idx_varid, ship_idx)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, record_idx_varid, record_idx)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, src_varid, src)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, lon_varid, lon)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, lat_varid, lat)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, p_varid, p)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, Ta_varid, Ta)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, sst_varid, sst)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, Td_varid, Td)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, rh_varid, rh)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, sh_varid, sh)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, wd_varid, wd)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, ws_varid, ws)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, wvh_varid, wvh)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, wvp_varid, wvp)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, svh_varid, svh)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, svp_varid, svp)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, svd_varid, svd)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, vis_varid, vis)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, cld_varid, cld)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, ice_varid, ice)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_close(ncid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    deallocate(time)
    deallocate(ship_name)
    deallocate(ship_idx)
    deallocate(record_idx)
    deallocate(src)
    deallocate(lon)
    deallocate(lat)
    deallocate(p)
    deallocate(Ta)
    deallocate(sst)
    deallocate(Td)
    deallocate(rh)
    deallocate(sh)
    deallocate(wd)
    deallocate(ws)
    deallocate(wvh)
    deallocate(wvp)
    deallocate(svh)
    deallocate(svp)
    deallocate(svd)
    deallocate(vis)
    deallocate(cld)
    deallocate(ice)

  end subroutine ship_netcdf_write

end module ship_netcdf_mod
