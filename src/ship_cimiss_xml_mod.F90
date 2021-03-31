module ship_cimiss_xml_mod

  use datetime
  use string
  use container
  use flogger
  use regex
  use fox_sax
  use params_mod
  use data_translators_utils_mod
  use ship_mod

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

    dummy_ships => ships
    dummy_records => records

    call log_notice('Reading ' // trim(file_path) // ' ...')

    call open_xml_file(xml, file_path, iostat)

    call parse(xml, startElement_handler=startElement_handler)

    call stop_parser(xml)

    call close_xml_t(xml)

    call log_notice('Ship size is ' // trim(to_str(ships%size)) // ', record size is ' // trim(to_str(records%size)) // '.')

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
    real ta, ta_qc
    real td, td_qc
    real rh, rh_qc
    real p, p_qc
    real sst, sst_qc
    real wd, wd_qc
    real ws, ws_qc
    real hww, hww_qc
    real pww, pww_qc
    real hsw, svh_qc
    real psw, psw_qc
    real dsw, dsw_qc
    real vis, vis_qc
    real clc, clc_qc
    integer i

    select case (name)
    case ('DS')
      do i = 1, getLength(attributes)
        select case (getQName(attributes, i))
        case ('requestParams')
          res = regex_search(getValue(attributes, i), 'datacode=([^&]*)&?')
          if (size(res) == 1) then
            if (res(1)%match(2)%str == 'OCEN_GLB_SHB') exit
          end if
          call log_error('Input file is not CIMISS OCEN_GLB_SHB!')
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
          read(value, *) ta
        case ('Q_TEM')
          read(value, *) ta_qc
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
          read(value, *) hww
        case ('Q_WinWave_Heigh')
          read(value, *) hww_qc
        case ('WinWave_CYC')
          read(value, *) pww
        case ('Q_WinWave_CYC')
          read(value, *) pww_qc
        case ('SeaWave_1st_Heigh')
          read(value, *) hsw
        case ('Q_SeaWave_1st_Heigh')
          read(value, *) svh_qc
        case ('SeaWave_1st_CYC')
          read(value, *) psw
        case ('Q_SeaWave_1st_CYC')
          read(value, *) psw_qc
        case ('SeaWave_1st_D')
          read(value, *) dsw
        case ('Q_SeaWave_1st_D')
          read(value, *) dsw_qc
        case ('VIS')
          read(value, *) vis
        case ('Q_VIS')
          read(value, *) vis_qc
        case ('CLO_Cov')
          read(value, *) clc
        case ('Q_CLO_Cov')
          read(value, *) clc_qc
        end select
      end do
      time = create_datetime(year, month, day, hour, minute)
      lon = merge(real_missing_value, lon, is_missing(lon, src='cimiss'))
      lat = merge(real_missing_value, lat, is_missing(lat, src='cimiss'))
      ta  = merge(real_missing_value, ta,  is_missing(ta,  src='cimiss'))
      td  = merge(real_missing_value, td,  is_missing(td,  src='cimiss'))
      rh  = merge(real_missing_value, rh,  is_missing(rh,  src='cimiss'))
      p   = merge(real_missing_value, p,   is_missing(p,   src='cimiss'))
      sst = merge(real_missing_value, sst, is_missing(sst, src='cimiss'))
      wd  = merge(real_missing_value, wd,  is_missing(wd,  src='cimiss'))
      ws  = merge(real_missing_value, ws,  is_missing(ws,  src='cimiss'))
      vis = merge(real_missing_value, vis, is_missing(vis, src='cimiss'))
      clc = merge(real_missing_value, clc, is_missing(clc, src='cimiss'))
      hww = merge(real_missing_value, hww, is_missing(hww, src='cimiss'))
      pww = merge(real_missing_value, pww, is_missing(pww, src='cimiss'))
      hsw = merge(real_missing_value, hsw, is_missing(hsw, src='cimiss'))
      psw = merge(real_missing_value, psw, is_missing(psw, src='cimiss'))
      dsw = merge(real_missing_value, dsw, is_missing(dsw, src='cimiss'))

      ! FIXME: CIMISS temperature units may be Kelvin!
      if (.not. is_missing(ta) .and. ta > 200) then
        ta = ta - freezing_point
        if (ta > 100) then
          call log_warning('Bad temperature value ' // to_str(ta, 10) // 'for ' // trim(ship_name) // '!')
          ta = real_missing_value
        else
          call log_warning('Convert temperature units from K to degC for ' // trim(ship_name) // '.')
        end if
      end if
      if (.not. is_missing(td) .and. td > 200) then
        td = td - freezing_point
        if (td > 100) then
          call log_warning('Bad dewpoint value ' // to_str(td, 10) // 'for ' // trim(ship_name) // '!')
          td = real_missing_value
        else
          call log_warning('Convert dewpoint units from K to degC for ' // trim(ship_name) // '.')
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
      record%p   = p
      record%ta  = ta
      record%sst = sst
      record%td  = td
      record%rh  = rh
      record%wd  = wd
      record%ws  = ws
      record%ua  = wind_u_component(ws, wd)
      record%va  = wind_v_component(ws, wd)
      record%hww = hww
      record%pww = pww
      record%hsw = hsw
      record%psw = psw
      record%dsw = dsw
      record%vis = vis
      record%clc = clc
      ! TODO: How to map CIMISS QC to PrepBUFR QC?
      record%ta_qc  = ta_qc
      record%sst_qc = sst_qc
      record%td_qc  = td_qc
      record%rh_qc  = rh_qc
      record%p_qc   = p_qc
      record%wd_qc  = wd_qc
      record%ws_qc  = ws_qc
      record%hww_qc = hww_qc
      record%pww_qc = pww_qc
      record%hsw_qc = svh_qc
      record%psw_qc = psw_qc
      record%dsw_qc = dsw_qc
      record%vis_qc = vis_qc
      record%clc_qc = clc_qc
      call dummy_records%insert(ship_name // '@' // time%isoformat(), record)
      call ship%records%insert(trim(to_str(record%seq_id)), record, nodup=.true.)
    end select

  end subroutine startElement_handler

end module ship_cimiss_xml_mod
