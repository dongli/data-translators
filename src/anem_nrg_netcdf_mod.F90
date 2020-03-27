module anem_nrg_netcdf_mod

  use netcdf
  use string
  use datetime
  use anem_nrg_mod
  use container
  use params_mod
  use utils_mod
  use qsort_mod

  implicit none

  private

  public anem_nrg_netcdf_write

contains

  subroutine anem_nrg_netcdf_write(file_path, towers, records)

    character(*), intent(inout) :: file_path
    type(hash_table_type), intent(in) :: towers
    type(linked_list_type), intent(in) :: records

    type(hash_table_iterator_type) tower_iterator
    type(linked_list_iterator_type) record_iterator
    type(hash_table_iterator_type) height_iterator
    type(hash_table_type) heights

    character(6), allocatable :: tower_names(:)
    real, allocatable :: lon(:)
    real, allocatable :: lat(:)
    real, allocatable :: elev(:)
    real(8), allocatable :: time(:)
    integer, allocatable :: record_tower_idx(:)
    real, allocatable :: h(:)
    real, allocatable ::  p_avg(:,:),  p_std(:,:),  p_min(:,:),  p_max(:,:)
    real, allocatable :: ua_avg(:,:), va_avg(:,:)
    real, allocatable :: ws_avg(:,:), ws_std(:,:), ws_min(:,:), ws_max(:,:)
    real, allocatable :: wd_avg(:,:), wd_std(:,:), wd_min(:,:), wd_max(:,:)
    real, allocatable :: ta_avg(:,:), ta_std(:,:), ta_min(:,:), ta_max(:,:)

    integer ncid, ierr, i, k, j, idx(1)
    integer tower_dimid, record_dimid, level_dimid, name_maxlen_dimid
    integer tower_name_varid
    integer lon_varid, lat_varid, elev_varid
    integer record_tower_idx_varid
    integer time_varid, h_varid
    integer  p_avg_varid,  p_std_varid,  p_min_varid,  p_max_varid
    integer ua_avg_varid, va_avg_varid
    integer ws_avg_varid, ws_std_varid, ws_min_varid, ws_max_varid
    integer wd_avg_varid, wd_std_varid, wd_min_varid, wd_max_varid
    integer ta_avg_varid, ta_std_varid, ta_min_varid, ta_max_varid

    if (file_path == '') file_path = 'anem_nrg.nc'

    heights = hash_table(100)

    tower_iterator = hash_table_iterator(towers)
    do while (.not. tower_iterator%ended())
      select type (tower => tower_iterator%value)
      type is (anem_nrg_tower_type)
        select type (record => tower%records%first_value())
        type is (anem_nrg_record_type)
          do i = 1, size(record%h)
            if (.not. heights%hashed(to_string(int(record%h(i))))) then
              call heights%insert(to_string(int(record%h(i))), record%h(i))
            end if
          end do
        end select
      end select
      call tower_iterator%next()
    end do

    allocate(tower_names(towers%size))
    allocate(lon(towers%size))
    allocate(lat(towers%size))
    allocate(elev(towers%size))
    allocate(h(heights%size))
    allocate(record_tower_idx(records%size))
    allocate(time(records%size))
    allocate( p_avg(heights%size,records%size));  p_avg = real_missing_value
    allocate( p_std(heights%size,records%size));  p_std = real_missing_value
    allocate( p_min(heights%size,records%size));  p_min = real_missing_value
    allocate( p_max(heights%size,records%size));  p_max = real_missing_value
    allocate(ua_avg(heights%size,records%size)); ua_avg = real_missing_value
    allocate(va_avg(heights%size,records%size)); va_avg = real_missing_value
    allocate(ws_avg(heights%size,records%size)); ws_avg = real_missing_value
    allocate(ws_std(heights%size,records%size)); ws_std = real_missing_value
    allocate(ws_min(heights%size,records%size)); ws_min = real_missing_value
    allocate(ws_max(heights%size,records%size)); ws_max = real_missing_value
    allocate(wd_avg(heights%size,records%size)); wd_avg = real_missing_value
    allocate(wd_std(heights%size,records%size)); wd_std = real_missing_value
    allocate(wd_min(heights%size,records%size)); wd_min = real_missing_value
    allocate(wd_max(heights%size,records%size)); wd_max = real_missing_value
    allocate(ta_avg(heights%size,records%size)); ta_avg = real_missing_value
    allocate(ta_std(heights%size,records%size)); ta_std = real_missing_value
    allocate(ta_min(heights%size,records%size)); ta_min = real_missing_value
    allocate(ta_max(heights%size,records%size)); ta_max = real_missing_value

    k = 1
    height_iterator = hash_table_iterator(heights)
    do while (.not. height_iterator%ended())
      select type (height => height_iterator%value)
      type is (real)
        h(k) = height
      end select
      k = k + 1
      call height_iterator%next()
    end do
    call qsort(h)

    i = 1
    tower_iterator = hash_table_iterator(towers)
    do while (.not. tower_iterator%ended())
      select type (tower => tower_iterator%value)
      type is (anem_nrg_tower_type)
        tower_names(i) = tower%name
        lon(i) = tower%lon
        lat(i) = tower%lat
        elev(i) = tower%z
      end select
      i = i + 1
      call tower_iterator%next()
    end do

    i = 1
    record_iterator = linked_list_iterator(records)
    do while (.not. record_iterator%ended())
      select type (record => record_iterator%value)
      type is (anem_nrg_record_type)
        time(i) = record%time%timestamp(timezone=0) / 3600.0
        idx = findloc(tower_names, record%tower%name)
        record_tower_idx(i) = idx(1)
        do k = 1, size(h)
          do j = 1, size(record%h)
            if (h(k) == record%h(j)) then
               p_avg(k,i) = record% p_avg(j)
               p_std(k,i) = record% p_std(j)
               p_min(k,i) = record% p_min(j)
               p_max(k,i) = record% p_max(j)
              ua_avg(k,i) = record%ua_avg(j)
              va_avg(k,i) = record%va_avg(j)
              ws_avg(k,i) = record%ws_avg(j)
              ws_std(k,i) = record%ws_std(j)
              ws_min(k,i) = record%ws_min(j)
              ws_max(k,i) = record%ws_max(j)
              wd_avg(k,i) = record%wd_avg(j)
              wd_std(k,i) = record%wd_std(j)
              wd_min(k,i) = record%wd_min(j)
              wd_max(k,i) = record%wd_max(j)
              ta_avg(k,i) = record%ta_avg(j)
              ta_std(k,i) = record%ta_std(j)
              ta_min(k,i) = record%ta_min(j)
              ta_max(k,i) = record%ta_max(j)
            end if
          end do
        end do
      end select
      i = i + 1
      call record_iterator%next()
    end do

    ierr = nf90_create(file_path, nf90_clobber, ncid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_dim(ncid, 'tower', towers%size, tower_dimid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_dim(ncid, 'record', records%size, record_dimid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_dim(ncid, 'level', heights%size, level_dimid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_dim(ncid, 'name_maxlen', 6, name_maxlen_dimid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'tower_name', nf90_char, [name_maxlen_dimid,tower_dimid], tower_name_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'lon', nf90_float, [tower_dimid], lon_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, lon_varid, 'units', 'degree_north')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'lat', nf90_float, [tower_dimid], lat_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, lat_varid, 'units', 'degree_east')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'elev', nf90_float, [tower_dimid], elev_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, elev_varid, 'units', 'm')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'record_tower_idx', nf90_int, [record_dimid], record_tower_idx_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'time', nf90_double, [record_dimid], time_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, time_varid, 'units', 'hours since 1970-01-01T00:00:00')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'h', nf90_float, [level_dimid], h_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, h_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, h_varid, 'units', 'm')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'p_avg', nf90_float, [level_dimid,record_dimid], p_avg_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, p_avg_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, p_avg_varid, 'units', 'hPa')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'p_std', nf90_float, [level_dimid,record_dimid], p_std_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, p_std_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, p_std_varid, 'units', 'hPa')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'p_min', nf90_float, [level_dimid,record_dimid], p_min_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, p_min_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, p_min_varid, 'units', 'hPa')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'p_max', nf90_float, [level_dimid,record_dimid], p_max_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, p_max_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, p_max_varid, 'units', 'hPa')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'ua_avg', nf90_float, [level_dimid,record_dimid], ua_avg_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, ua_avg_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, ua_avg_varid, 'units', 'm s-1')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'va_avg', nf90_float, [level_dimid,record_dimid], va_avg_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, va_avg_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, va_avg_varid, 'units', 'm s-1')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'ws_avg', nf90_float, [level_dimid,record_dimid], ws_avg_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, ws_avg_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, ws_avg_varid, 'units', 'm s-1')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'ws_std', nf90_float, [level_dimid,record_dimid], ws_std_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, ws_std_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, ws_std_varid, 'units', 'm s-1')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'ws_min', nf90_float, [level_dimid,record_dimid], ws_min_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, ws_min_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, ws_min_varid, 'units', 'm s-1')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'ws_max', nf90_float, [level_dimid,record_dimid], ws_max_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, ws_max_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, ws_max_varid, 'units', 'm s-1')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'wd_avg', nf90_float, [level_dimid,record_dimid], wd_avg_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, wd_avg_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, wd_avg_varid, 'units', 'm s-1')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'wd_std', nf90_float, [level_dimid,record_dimid], wd_std_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, wd_std_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, wd_std_varid, 'units', 'm s-1')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'wd_min', nf90_float, [level_dimid,record_dimid], wd_min_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, wd_min_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, wd_min_varid, 'units', 'm s-1')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'wd_max', nf90_float, [level_dimid,record_dimid], wd_max_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, wd_max_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, wd_max_varid, 'units', 'm s-1')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'ta_avg', nf90_float, [level_dimid,record_dimid], ta_avg_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, ta_avg_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, ta_avg_varid, 'units', 'degC')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'ta_std', nf90_float, [level_dimid,record_dimid], ta_std_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, ta_std_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, ta_std_varid, 'units', 'degC')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'ta_min', nf90_float, [level_dimid,record_dimid], ta_min_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, ta_min_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, ta_min_varid, 'units', 'degC')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_def_var(ncid, 'ta_max', nf90_float, [level_dimid,record_dimid], ta_max_varid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, ta_max_varid, '_FillValue', real_missing_value)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_att(ncid, ta_max_varid, 'units', 'degC')
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_enddef(ncid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, tower_name_varid, tower_names)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, lon_varid, lon)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, lat_varid, lat)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, elev_varid, elev)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, record_tower_idx_varid, record_tower_idx)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, time_varid, time)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, h_varid, h)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, p_avg_varid, p_avg)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, p_std_varid, p_std)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, p_min_varid, p_min)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, p_max_varid, p_max)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, ua_avg_varid, ua_avg)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, va_avg_varid, va_avg)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, ws_avg_varid, ws_avg)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, ws_std_varid, ws_std)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, ws_min_varid, ws_min)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, ws_max_varid, ws_max)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, wd_avg_varid, wd_avg)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, wd_std_varid, wd_std)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, wd_min_varid, wd_min)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, wd_max_varid, wd_max)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, ta_avg_varid, ta_avg)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, ta_std_varid, ta_std)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, ta_min_varid, ta_min)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_put_var(ncid, ta_max_varid, ta_max)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    ierr = nf90_close(ncid)
    call handle_netcdf_error(ierr, __FILE__, __LINE__)

    deallocate(tower_names)
    deallocate(lon)
    deallocate(lat)
    deallocate(elev)
    deallocate(h)
    deallocate(time)
    deallocate( p_avg)
    deallocate( p_std)
    deallocate( p_min)
    deallocate( p_max)
    deallocate(ua_avg)
    deallocate(va_avg)
    deallocate(ws_avg)
    deallocate(ws_std)
    deallocate(ws_min)
    deallocate(ws_max)
    deallocate(wd_avg)
    deallocate(wd_std)
    deallocate(wd_min)
    deallocate(wd_max)
    deallocate(ta_avg)
    deallocate(ta_std)
    deallocate(ta_min)
    deallocate(ta_max)

  end subroutine anem_nrg_netcdf_write

end module anem_nrg_netcdf_mod
