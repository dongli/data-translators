module ship_cimiss_xml_mod

  use ship_mod
  use datetime_mod
  use timedelta_mod
  use hash_table_mod
  use linked_list_mod
  use regex
  use fox_sax
  use string_mod
  use params_mod
  use utils_mod

  implicit none

  private

  public ship_cimiss_xml_read

  type(hash_table_type), pointer :: dummy_ships
  type(linked_list_type), pointer :: dummy_records

contains

  subroutine ship_cimiss_xml_read(file_path, ships, records)

    character(*), intent(in) :: file_path
    type(hash_table_type), intent(inout), target :: ships
    type(linked_list_type), intent(inout), target :: records

    type(xml_t) xml
    integer i, iostat

    ships = hash_table(chunk_size=50000, max_load_factor=0.9)
    call records%clear()

    dummy_ships => ships
    dummy_records => records

    write(*, *) '[Notice]: Reading ' // trim(file_path) // ' ...'

    call open_xml_file(xml, file_path, iostat)

    call parse(xml, startElement_handler=startElement_handler)

    call stop_parser(xml)

    call close_xml_t(xml)

    write(*, *) '[Notice]: Ship size is ' // trim(to_string(ships%size)) // ', record size is ' // trim(to_string(records%size)) // '.'

  end subroutine ship_cimiss_xml_read

  subroutine startElement_handler(uri, local_name, name, attributes)

    character(*), intent(in) :: uri
    character(*), intent(in) :: local_name
    character(*), intent(in) :: name
    type(dictionary_t), intent(in) :: attributes

    type(reg_matches), allocatable :: res(:)
    type(ship_type), pointer :: ship
    type(ship_record_type), pointer :: record
    character(10) ship_name
    character(256) value
    real lat, lon
    integer year, month, day, hour, minute
    type(datetime_type) time
    real t, t_qc
    real td, td_qc
    real rh, rh_qc
    real p, p_qc
    real sst, sst_qc
    real wd, wd_qc
    real ws, ws_qc
    real wvh, wvh_qc
    real wvp, wvp_qc
    real svh, svh_qc
    real svp, svp_qc
    real svd, svd_qc
    real vis, vis_qc
    real cld, cld_qc
    integer i

    select case (name)
    case ('DS')
      do i = 1, getLength(attributes)
        select case (getQName(attributes, i))
        case ('requestParamstimes')
          res = regex_search(getValue(attributes, i), 'datacode=([^&]*)&')
          if (size(res) /= 1 .or. res(1)%match(2)%str /= 'OCEN_GLB_SHB') then
            write(*, *) '[Error]: Input file is not CIMISS OCEN_GLB_SHB!'
            stop 1
          end if 
        end select
      end do
    case ('R')
      do i = 1, getLength(attributes)
        value = getValue(attributes, i)
        select case (getQName(attributes, i))
        case ('Station_Id_C')
          ship_name = value
        case ('Lat')
          read(value, *) lat
        case ('Lon')
          read(value, *) lon
        case ('Year')
          read(value, *) year
        case ('Mon')
          read(value, *) month
        case ('Day')
          read(value, *) day
        case ('Hour')
          read(value, *) hour
        case ('Min')
          read(value, *) minute
        case ('TEM')
          read(value, *) t
        case ('Q_TEM')
          read(value, *) t_qc
        case ('DPT')
          read(value, *) td
        case ('Q_DPT')
          read(value, *) td_qc
        case ('RHU')
          read(value, *) rh
        case ('Q_RHU')
          read(value, *) rh_qc
        case ('PRS_Sea')
          read(value, *) p
        case ('Q_PRS_Sea')
          read(value, *) p_qc
        case ('SST')
          read(value, *) sst
        case ('Q_SST')
          read(value, *) sst_qc
        case ('WIN_D')
          read(value, *) wd
        case ('Q_WIN_D')
          read(value, *) wd_qc
        case ('WIN_S')
          read(value, *) ws
        case ('Q_WIN_S')
          read(value, *) ws_qc
        case ('WinWave_Heigh')
          read(value, *) wvh
        case ('Q_WinWave_Heigh')
          read(value, *) wvh_qc
        case ('WinWave_CYC')
          read(value, *) wvp
        case ('Q_WinWave_CYC')
          read(value, *) wvp_qc
        case ('SeaWave_1st_Heigh')
          read(value, *) svh
        case ('Q_SeaWave_1st_Heigh')
          read(value, *) svh_qc
        case ('SeaWave_1st_CYC')
          read(value, *) svp
        case ('Q_SeaWave_1st_CYC')
          read(value, *) svp_qc
        case ('SeaWave_1st_D')
          read(value, *) svd
        case ('Q_SeaWave_1st_D')
          read(value, *) svd_qc
        case ('VIS')
          read(value, *) vis
        case ('Q_VIS')
          read(value, *) vis_qc
        case ('CLO_Cov')
          read(value, *) cld
        case ('Q_CLO_Cov')
          read(value, *) cld_qc
        end select
      end do
      time = create_datetime(year, month, day, hour, minute)
      lon = merge(real_missing_value, lon, is_missing(lon, src='cimiss'))
      lat = merge(real_missing_value, lat, is_missing(lat, src='cimiss'))
      t   = merge(real_missing_value, t,   is_missing(t,   src='cimiss'))
      td  = merge(real_missing_value, td,  is_missing(td,  src='cimiss'))
      rh  = merge(real_missing_value, rh,  is_missing(rh,  src='cimiss'))
      p   = merge(real_missing_value, p,   is_missing(p,   src='cimiss'))
      sst = merge(real_missing_value, sst, is_missing(sst, src='cimiss'))
      wd  = merge(real_missing_value, wd,  is_missing(wd,  src='cimiss'))
      ws  = merge(real_missing_value, ws,  is_missing(ws,  src='cimiss'))
      vis = merge(real_missing_value, vis, is_missing(vis, src='cimiss'))
      cld = merge(real_missing_value, cld, is_missing(cld, src='cimiss'))
      wvh = merge(real_missing_value, wvh, is_missing(wvh, src='cimiss'))
      wvp = merge(real_missing_value, wvp, is_missing(wvp, src='cimiss'))
      svh = merge(real_missing_value, svh, is_missing(svh, src='cimiss'))
      svp = merge(real_missing_value, svp, is_missing(svp, src='cimiss'))
      svd = merge(real_missing_value, svd, is_missing(svd, src='cimiss'))

      ! FIXME: CIMISS temperature units may be Kelvin!
      if (.not. is_missing(t) .and. t > 200) then
        t = t - freezing_point
        if (t > 100) then
          write(*, *) '[Warning]: Bad temperature value ', t, 'for ', trim(ship_name), '!'
          t = real_missing_value
        else
          write(*, *) '[Warning]: Convert temperature units from K to degC for ', trim(ship_name), '.'
        end if
      end if
      if (.not. is_missing(td) .and. td > 200) then
        td = td - freezing_point
        if (td > 100) then
          write(*, *) '[Warning]: Bad dewpoint value ', td, 'for ', trim(ship_name), '!'
          td = real_missing_value
        else
          write(*, *) '[Warning]: Convert dewpoint units from K to degC for ', trim(ship_name), '.'
        end if
      end if
      ! Create ship and record.
      if (dummy_ships%hashed(ship_name)) then
        select type (value => dummy_ships%value(ship_name))
        type is (ship_type)
          ship => value
        end select
      else
        allocate(ship)
        call ship%init(ship_name)
        ship%seq_id = dummy_ships%size
        call dummy_ships%insert(ship_name, ship)
      end if
      allocate(record)
      record%seq_id = dummy_records%size
      record%ship => ship
      record%time = time
      ! Set record.
      record%source = 'CIMISS'
      record%lon = lon
      record%lat = lat
      record%air_temperature = t
      record%sea_temperature = sst
      record%dewpoint = td
      record%relative_humidity = rh
      record%pressure = p
      record%wind_direction = wd
      record%wind_speed = ws
      record%wind_u = wind_u_component(ws, wd)
      record%wind_v = wind_v_component(ws, wd)
      record%wind_wave_height = wvh
      record%wind_wave_period = wvp
      record%surge_wave_height = svh
      record%surge_wave_period = svp
      record%surge_wave_direction = svd
      record%visibility = vis
      record%cloud_cover = cld
      ! TODO: How to map CIMISS QC to PrepBUFR QC?
      record%air_temperature_qc = t_qc
      record%sea_temperature_qc = sst_qc
      record%dewpoint_qc = td_qc
      record%relative_humidity_qc = rh_qc
      record%pressure_qc = p_qc
      if (wd_qc /= 0) then
        record%wind_qc = wd_qc
      else if (ws_qc /= 0) then
        record%wind_qc = ws_qc
      else
        record%wind_qc = 0
      end if
      record%wind_wave_height_qc = wvh_qc
      record%wind_wave_period_qc = wvp_qc
      record%surge_wave_height_qc = svh_qc
      record%surge_wave_period_qc = svp_qc
      record%surge_wave_direction_qc = svd_qc
      record%visibility_qc = vis_qc
      record%cloud_cover_qc = cld_qc
      call dummy_records%insert(ship_name // '@' // time%isoformat(), record)
      call ship%records%insert(trim(to_string(record%seq_id)), record, nodup=.true.)
    end select

  end subroutine startElement_handler

end module ship_cimiss_xml_mod
