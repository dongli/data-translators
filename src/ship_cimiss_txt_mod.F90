module ship_cimiss_txt_mod

  use datetime
  use ship_mod
  use hash_table_mod
  use linked_list_mod
  use regex
  use params_mod
  use utils_mod

  implicit none

  private

  public ship_cimiss_txt_read

contains

  subroutine ship_cimiss_txt_read(file_path, ships, records)

    character(*), intent(in) :: file_path
    type(hash_table_type), intent(inout) :: ships
    type(linked_list_type), intent(inout) :: records

    character(1024) line
    integer i, j, iostat
    type(reg_matches), allocatable :: res(:)
    character(30), allocatable :: elements(:)
    character(10) ship_name
    real lat, lon
    real ws,  ws_qc
    real wd,  wd_qc
    real ta,  ta_qc
    real td,  td_qc
    real rh,  rh_qc
    real p,   p_qc
    real slp, slp_qc
    real sst, sst_qc
    integer type, year, month, day, hour, min
    type(datetime_type) time
    logical new_record
    type(ship_type), pointer :: ship
    type(ship_record_type), pointer :: record

    ships = hash_table(chunk_size=50000, max_load_factor=0.9)
    call records%clear()

    open(10, file=file_path, action='read', form='formatted')
    ! Header line
    read(10, '(A)') line
    res = regex_search(line, 'returnMessage="([^"]*)"')
    if (size(res) /= 1) then
      write(*, *) '[Error]: Bad CIMISS text file without matched returnMessage token!'
      stop 1
    else if (res(1)%match(2)%str /= 'Query Succeed') then
      write(*, *) '[Error]: Failed CIMISS query! ' // trim(res(1)%match(2)%str)
      stop 1
    end if
    ! Elements line
    read(10, '(A)') line
    res = regex_search(line, '(\w+)')
    if (size(res) > 0) then
      allocate(elements(size(res)))
      do i = 1, size(res)
        elements(i) = res(i)%match(1)%str
      end do
    else
      write(*, *) '[Error]: Bad CIMISS text file without matched elements line!'
      stop 1
    end if
    ! Record line
    do while (.true.)
      read(10, '(A)', iostat=iostat) line
      res = regex_search(line, '([^\s]+)')
      if (iostat /= 0) exit
      ship_name = ''
      lat    = real_missing_value
      lon    = real_missing_value
      year   = int_missing_value
      month  = int_missing_value
      day    = int_missing_value
      hour   = int_missing_value
      min    = int_missing_value
      wd     = real_missing_value
      wd_qc  = int_missing_value
      ws     = real_missing_value
      ws_qc  = int_missing_value
      ta     = real_missing_value
      ta_qc  = int_missing_value
      td     = real_missing_value
      td_qc  = int_missing_value
      rh     = real_missing_value
      rh_qc  = int_missing_value
      p      = real_missing_value
      p_qc   = int_missing_value
      slp    = real_missing_value
      slp_qc = int_missing_value
      sst    = real_missing_value
      sst_qc = int_missing_value
      do i = 1, size(elements)
        select case (elements(i))
        case ('Station_Id_C')
          ship_name = res(i)%match(1)%str
        case ('Lat')
          read(res(i)%match(1)%str, *) lat
        case ('Lon')
          read(res(i)%match(1)%str, *) lon
        case ('Station_Type')
          read(res(i)%match(1)%str, *) type
        case ('Year')
          read(res(i)%match(1)%str, *) year
        case ('Mon')
          read(res(i)%match(1)%str, *) month
        case ('Day')
          read(res(i)%match(1)%str, *) day
        case ('Hour')
          read(res(i)%match(1)%str, *) hour
        case ('Min')
          read(res(i)%match(1)%str, *) min
        case ('WIN_D')
          read(res(i)%match(1)%str, *) wd
        case ('Q_WIN_D')
          read(res(i)%match(1)%str, *) wd_qc
        case ('WIN_S')
          read(res(i)%match(1)%str, *) ws
        case ('Q_WIN_S')
          read(res(i)%match(1)%str, *) ws_qc
        case ('TEM')
          read(res(i)%match(1)%str, *) ta
        case ('Q_TEM')
          read(res(i)%match(1)%str, *) ta_qc
        case ('DPT')
          read(res(i)%match(1)%str, *) td
        case ('Q_DPT')
          read(res(i)%match(1)%str, *) td_qc
        case ('RHU')
          read(res(i)%match(1)%str, *) rh
        case ('Q_RHU')
          read(res(i)%match(1)%str, *) rh_qc
        case ('PRS')
          read(res(i)%match(1)%str, *) p
        case ('Q_PRS')
          read(res(i)%match(1)%str, *) p_qc
        case ('PRS_Sea')
          read(res(i)%match(1)%str, *) slp
        case ('Q_PRS_Sea')
          read(res(i)%match(1)%str, *) slp_qc
        case ('sst')
          read(res(i)%match(1)%str, *) sst
        case ('Q_sst')
          read(res(i)%match(1)%str, *) sst_qc
        end select
      end do
      time = create_datetime(year, month, day, hour, min)
      if (ships%hashed(ship_name)) then
        select type (value => ships%value(ship_name))
        type is (ship_type)
          ship => value
        end select
      else
        allocate(ship)
        ship%name = ship_name
        call ships%insert(ship_name, ship)
      end if
      nullify(record)
      select type (value => records%last_value())
      type is (ship_record_type)
        record => value
        if (record%ship%name == ship_name .and. record%time == time .and. &
            record%lon == lon .and. record%lat == lat) then
          new_record = .false.
        else
          nullify(record)
        end if
      end select
      if (.not. associated(record)) then
        allocate(record)
        record%ship => ship
        record%time = time
        new_record = .true.
      end if
      record%time   = time
      record%lon    = lon
      record%lat    = lat
      record%p      = p
      record%p_qc   = p_qc
      record%ta     = ta
      record%ta_qc  = ta_qc
      record%sst    = sst
      record%sst_qc = sst_qc
      record%td     = td
      record%td_qc  = td_qc
      record%rh     = rh
      record%rh_qc  = rh_qc
      record%ws     = ws
      record%ws_qc  = ws_qc
      record%wd     = wd
      record%wd_qc  = wd_qc

      if (new_record) then
        call records%insert(ship_name // '@' // time%isoformat(), record)
      end if
      ! if (ship_name == '40718') call record%print()
    end do
    close(10)

  end subroutine ship_cimiss_txt_read

end module ship_cimiss_txt_mod
