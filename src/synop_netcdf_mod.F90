module synop_netcdf_mod

  use netcdf
  use synop_mod

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
    integer station_dimid
    integer station_name_maxlen_dimid
    integer record_dimid
    integer station_name_varid
    integer station_lon_varid
    integer station_lat_varid
    integer station_z_varid

    character(8), allocatable :: station_names(:)
    real, allocatable :: station_lons(:)
    real, allocatable :: station_lats(:)
    real, allocatable :: station_zs(:)
    integer i

    if (file_path == '') file_path ='synop.nc'

    write(*, *) '[Notice]: Writing ' // trim(file_path) // ' ...'

    ierr = nf90_create(file_path, nf90_clobber, ncid)

    ierr = nf90_def_dim(ncid, 'station', stations%size, station_dimid)

    ierr = nf90_def_dim(ncid, 'station_name_maxlen_dimid', 8, station_name_maxlen_dimid)

    ierr = nf90_def_dim(ncid, 'record', records%size, record_dimid)

    ierr = nf90_def_var(ncid, 'station_name', nf90_string, [station_name_maxlen_dimid, station_dimid], station_name_varid)

    ierr = nf90_def_var(ncid, 'station_lon', nf90_float, [station_dimid], station_lon_varid)

    ierr = nf90_def_var(ncid, 'station_lat', nf90_float, [station_dimid], station_lat_varid)

    ierr = nf90_def_var(ncid, 'station_z', nf90_float, [station_dimid], station_z_varid)

    ierr = nf90_enddef(ncid)

    allocate(station_names(stations%size))
    allocate(station_lons(stations%size))
    allocate(station_lats(stations%size))
    allocate(station_zs(stations%size))

    i = 1
    station_iterator = hash_table_iterator(stations)
    do while (.not. station_iterator%ended())
      select type (station => station_iterator%value)
      type is (synop_station_type)
        station_names(i) = station%name
        station_lons(i)  = station%lon
        station_lats(i)  = station%lat
        station_zs(i)    = station%z
      end select
      i = i + 1
      call station_iterator%next()
    end do

    ierr = nf90_put_var(ncid, station_name_varid, station_names)

    ierr = nf90_put_var(ncid, station_lon_varid, station_lons)

    ierr = nf90_put_var(ncid, station_lat_varid, station_lats)

    ierr = nf90_put_var(ncid, station_z_varid, station_zs)

    ierr = nf90_close(ncid)

  end subroutine synop_netcdf_write

end module synop_netcdf_mod