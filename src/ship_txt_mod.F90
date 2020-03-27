module ship_txt_mod

  use datetime
  use string
  use container
  use flogger
  use regex
  use params_mod
  use utils_mod
  use ship_mod

  implicit none

  private

  public ship_txt_read

contains

  subroutine ship_txt_read(file_path, ships, records)

    character(*), intent(in) :: file_path
    type(hash_table_type), intent(inout) :: ships
    type(linked_list_type), intent(inout) :: records

    type(ship_type), pointer :: ship
    type(ship_record_type), pointer :: record
    type(datetime_type) time
    character(1024) line
    integer ierr
    integer source
    integer year
    integer month
    integer day
    integer hour
    integer minute
    real lat
    real lon
    character(20) site_name
    integer site_type

    real move_dir ! deg
    real move_spd ! m/s
    real p        ! Pa
    real ta       ! degC
    real td       ! degC
    real rh       ! %
    real wd       ! deg
    real ws       ! m/s
    real sst      ! degC
    integer ice
    real ua       ! m/s
    real va       ! m/s
    real sh       ! mg/kg
    real alt      ! m
    
    integer   p_qc
    integer  ta_qc
    integer  td_qc
    integer  rh_qc
    integer  wd_qc
    integer  ws_qc
    integer sst_qc
    integer ice_qc
    integer  ua_qc
    integer  va_qc
    integer  sh_qc
    integer enc_qc
    integer wind_qc ! FIXME: Merge wind_*_qc into one QC mark.
 
    call log_notice('Reading ' // trim(file_path) // ' ...')

    open(10, file=file_path, action='read', form='formatted')
    ! Header line
    read(10, '(A)') line
    do while (.true.)
      read(10, '(A)', iostat=ierr) line
      if (ierr == 0) then
        read(line, *) source,    & ! 1
                      year,      & ! 2
                      month,     & ! 3
                      day,       & ! 4
                      hour,      & ! 5
                      minute,    & ! 6
                      lat,       & ! 7
                      lon,       & ! 8
                      site_name, & ! 9
                      site_type, & ! 10
                      move_dir,  & ! 11
                      move_spd,  & ! 12
                      p,         & ! 13
                      ta,        & ! 14
                      td,        & ! 15
                      rh,        & ! 16
                      wd,        & ! 17
                      ws,        & ! 18
                      sst,       & ! 19
                      ice,       & ! 20
                      ua,        & ! 21
                      va,        & ! 22
                      sh,        & ! 23
                      alt,       & ! 24
                      p_qc,      & ! 25
                      ta_qc,     & ! 26
                      td_qc,     & ! 27
                      rh_qc,     & ! 28
                      wd_qc,     & ! 29
                      ws_qc,     & ! 30
                      sst_qc,    & ! 31
                      ice_qc,    & ! 32
                      ua_qc,     & ! 33
                      va_qc,     & ! 34
                      sh_qc,     & ! 35
                      enc_qc       ! 36

        time      = create_datetime(year=year, month=month, day=day, hour=hour, minute=minute)
        source    = merge(int_missing_value, source, is_missing(source, src='cimiss'))
        lat       = merge(real_missing_value, lat, is_missing(lat, src='cimiss'))
        lon       = merge(real_missing_value, lon, is_missing(lon, src='cimiss'))
        site_type = merge(int_missing_value, site_type, is_missing(site_type, src='cimiss'))
        move_dir  = merge(real_missing_value, move_dir, is_missing(move_dir, src='cimiss'))
        move_spd  = merge(real_missing_value, move_spd, is_missing(move_spd, src='cimiss'))
        p      = multiply(merge(real_missing_value, p, is_missing(p, src='cimiss')), 100.0)
        ta     = merge(real_missing_value, ta, is_missing(ta, src='cimiss'))
        td     = merge(real_missing_value, td, is_missing(td, src='cimiss'))
        rh     = merge(real_missing_value, rh, is_missing(rh, src='cimiss'))
        wd     = merge(real_missing_value, wd, is_missing(wd, src='cimiss'))
        ws     = merge(real_missing_value, ws, is_missing(ws, src='cimiss'))
        sst    = merge(real_missing_value, sst, is_missing(sst, src='cimiss'))
        ice    = merge(int_missing_value, ice, is_missing(ice, src='cimiss'))
        ua     = merge(real_missing_value, ua, is_missing(ua, src='cimiss'))
        va     = merge(real_missing_value, va, is_missing(va, src='cimiss'))
        sh     = merge(real_missing_value, sh, is_missing(sh, src='cimiss'))
        alt    = merge(real_missing_value, alt, is_missing(alt, src='cimiss'))
        p_qc   = merge(int_missing_value, p_qc, is_missing(p_qc, src='cimiss'))
        ta_qc  = merge(int_missing_value, ta_qc, is_missing(ta_qc, src='cimiss'))
        td_qc  = merge(int_missing_value, td_qc, is_missing(td_qc, src='cimiss'))
        rh_qc  = merge(int_missing_value, rh_qc, is_missing(rh_qc, src='cimiss'))
        wd_qc  = merge(int_missing_value, wd_qc, is_missing(wd_qc, src='cimiss'))
        ws_qc  = merge(int_missing_value, ws_qc, is_missing(ws_qc, src='cimiss'))
        ua_qc  = merge(int_missing_value, ua_qc, is_missing(ua_qc, src='cimiss'))
        va_qc  = merge(int_missing_value, va_qc, is_missing(ua_qc, src='cimiss'))
        sst_qc = merge(int_missing_value, sst_qc, is_missing(sst_qc, src='cimiss'))
        ice_qc = merge(int_missing_value, ice_qc, is_missing(ice_qc, src='cimiss'))
        sh_qc  = merge(int_missing_value, sh_qc, is_missing(sh_qc, src='cimiss'))
        enc_qc = merge(int_missing_value, enc_qc, is_missing(enc_qc, src='cimiss'))
        ! Create ship and record.
        if (ships%hashed(site_name)) then
          select type (value => ships%value(site_name))
          type is (ship_type)
            ship => value
          end select
        else
          allocate(ship)
          call ship%init(site_name)
          ship%seq_id = ships%size
          call ships%insert(site_name, ship)
        end if
        allocate(record)
        record%seq_id = records%size
        record%ship => ship
        record%time = time
        ! Set record.
        select case (source)
        case (1)
          record%source = 'GTS'
        case (2)
          record%source = 'ICOADS'
        case (3)
          record%source = 'CFSRGDAS'
        case (4)
          record%source = 'CMA BUOY'
        case (5)
          record%source = 'CHN SHIP'
        case (6)
          record%source = 'CMAILAWS'
        case (9)
          record%source = 'MULTISRC'
        end select
        record%lon    = lon
        record%lat    = lat
        record%p      = p
        record%ta     = ta
        record%sst    = sst
        record%td     = td
        record%rh     = rh
        record%sh     = sh
        record%ua     = ua
        record%va     = va
        record%wd     = wd
        record%ws     = ws
        record%ice    = ice
        ! TODO: How to map CIMISS QC to PrepBUFR QC?
        record%p_qc   = p_qc
        record%ta_qc  = ta_qc
        record%sst_qc = sst_qc
        record%td_qc  = td_qc
        record%rh_qc  = rh_qc
        record%sh_qc  = sh_qc
        record%ua_qc  = ua_qc
        record%va_qc  = va_qc
        record%ice_qc = ice_qc
        call records%insert(site_name // '@' // time%isoformat(), record)
        call ship%records%insert(trim(to_string(record%seq_id)), record, nodup=.true.)
      else
        exit
      end if
    end do
    close(10)

    call log_notice('Ship size is ' // trim(to_string(ships%size)) // ', record size is ' // trim(to_string(records%size)) // '.')

  end subroutine ship_txt_read

end module ship_txt_mod
