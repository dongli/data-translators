module ship_netcdf_mod

  use netcdf
  use ship_mod
  use data_translators_utils_mod

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
    integer ta_varid
    integer sst_varid
    integer td_varid
    integer rh_varid
    integer sh_varid
    integer u_varid
    integer v_varid
    integer wd_varid
    integer ws_varid
    integer pww_varid
    integer hww_varid
    integer psw_varid
    integer hsw_varid
    integer dsw_varid
    integer vis_varid
    integer clc_varid
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
    real, allocatable :: ta(:)
    real, allocatable :: sst(:)
    real, allocatable :: td(:)
    real, allocatable :: rh(:)
    real, allocatable :: sh(:)
    real, allocatable :: u(:)
    real, allocatable :: v(:)
    real, allocatable :: wd(:)
    real, allocatable :: ws(:)
    real, allocatable :: hww(:)
    real, allocatable :: pww(:)
    real, allocatable :: hsw(:)
    real, allocatable :: psw(:)
    real, allocatable :: dsw(:)
    real, allocatable :: vis(:)
    real, allocatable :: clc(:)
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

    ierr = nf90_def_var(ncid, 'p', nf90_float, [record_dimid], p_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, p_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, p_varid, 'units', 'hPa')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'ta', nf90_float, [record_dimid], ta_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, ta_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, ta_varid, 'units', 'degC')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'sst', nf90_float, [record_dimid], sst_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, sst_varid, 'units', 'degC')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, sst_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'td', nf90_float, [record_dimid], td_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, td_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, td_varid, 'units', 'degC')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'rh', nf90_float, [record_dimid], rh_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, rh_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, rh_varid, 'units', '%')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'sh', nf90_float, [record_dimid], sh_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, sh_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, sh_varid, 'units', 'mg kg-1')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'wd', nf90_float, [record_dimid], wd_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, wd_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'ws', nf90_float, [record_dimid], ws_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, ws_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'hww', nf90_float, [record_dimid], hww_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, hww_varid, 'units', 'm')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, hww_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'pww', nf90_float, [record_dimid], pww_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, pww_varid, 'units', 's')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, pww_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'hsw', nf90_float, [record_dimid], hsw_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, hsw_varid, 'units', 'm')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, hsw_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'psw', nf90_float, [record_dimid], psw_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, psw_varid, 'units', 's')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, psw_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'dsw', nf90_float, [record_dimid], dsw_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, dsw_varid, 'units', 'deg')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, dsw_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'vis', nf90_float, [record_dimid], vis_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, vis_varid, 'units', 'm')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, vis_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'clc', nf90_float, [record_dimid], clc_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, clc_varid, 'units', '%')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, clc_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'ice', nf90_float, [record_dimid], ice_varid)
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
    allocate(ta(records%size))
    allocate(sst(records%size))
    allocate(td(records%size))
    allocate(rh(records%size))
    allocate(sh(records%size))
    allocate(wd(records%size))
    allocate(ws(records%size))
    allocate(hww(records%size))
    allocate(pww(records%size))
    allocate(hsw(records%size))
    allocate(psw(records%size))
    allocate(dsw(records%size))
    allocate(vis(records%size))
    allocate(clc(records%size))
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
          write(*, *) '[Error]: Exceeds max_record_per_ship ' // trim(to_str(max_record_per_ship)) // '!'
          stop 1
        end if
        record_idx(j,ship_idx(i)+1) = record%seq_id
        src(i) = record%source
        lon(i) = record%lon
        lat(i) = record%lat
        p  (i) = record%p
        ta (i) = record%ta
        sst(i) = record%sst
        td (i) = record%td
        rh (i) = record%rh
        sh (i) = record%sh
        wd (i) = record%wd
        ws (i) = record%ws
        hww(i) = record%hww
        pww(i) = record%pww
        hsw(i) = record%hsw
        psw(i) = record%psw
        dsw(i) = record%dsw
        vis(i) = record%vis
        clc(i) = record%clc
        ice(i) = record%ice
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

    ierr = nf90_put_var(ncid, ta_varid, ta)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, sst_varid, sst)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, td_varid, td)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, rh_varid, rh)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, sh_varid, sh)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, wd_varid, wd)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, ws_varid, ws)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, hww_varid, hww)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, pww_varid, pww)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, hsw_varid, hsw)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, psw_varid, psw)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, dsw_varid, dsw)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, vis_varid, vis)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, clc_varid, clc)
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
    deallocate(ta)
    deallocate(sst)
    deallocate(td)
    deallocate(rh)
    deallocate(sh)
    deallocate(wd)
    deallocate(ws)
    deallocate(hww)
    deallocate(pww)
    deallocate(hsw)
    deallocate(psw)
    deallocate(dsw)
    deallocate(vis)
    deallocate(clc)
    deallocate(ice)

  end subroutine ship_netcdf_write

end module ship_netcdf_mod
