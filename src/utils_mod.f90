module utils_mod

  use params_mod
  use eccodes

  implicit none

contains

  real function wind_direction(u, v) result(wd)

    real, intent(in) :: u
    real, intent(in) :: v

    real angle

    wd = atan2(u, v) * degree + 180.0

    if (wd >= 360) wd = wd - 360.0d0
    if (wd < 0) wd = wd + 360.0d0

  end function wind_direction

  real function wind_u_component(ws, wd) result(u)

    real, intent(in) :: ws
    real, intent(in) :: wd

    u = - ws * sin(wd * radian)

  end function wind_u_component

  real function wind_v_component(ws, wd) result(v)

    real, intent(in) :: ws
    real, intent(in) :: wd

    v = - ws * cos(wd * radian)

  end function wind_v_component

  real function prepbufr_raw(stack, qc, pc) result(res)

    real(8), intent(in) :: stack(:)
    real(8), intent(in), optional :: qc(:)
    real(8), intent(in), optional :: pc(:)

    integer i

    res = real_missing_value
    if (present(qc)) then
      do i = 1, size(stack)
        if (qc(i) <= 2) then
          res = stack(i)
          exit
        end if
      end do
    else if (present(pc)) then
      do i = 1, size(stack)
        if (pc(i) == 1) then
          res = stack(i)
          exit
        else if (pc(i) == missing_value_in_prepbufr) then
          exit
        end if
      end do
    else
      do i = 1, size(stack)
        if (stack(i) == missing_value_in_prepbufr) then
          if (i /= 1) res = stack(i-1)
          exit
        end if
      end do
    end if

  end function prepbufr_raw

  function prepbufr_codes(codes) result(res)

    real(8), intent(in) :: codes(:)
    integer res(size(codes))

    integer i

    res(:) = codes(:)
    do i = 1, size(codes)
      if (res(i) < 0) then
        res(i) = int_missing_value
      end if
    end do

  end function prepbufr_codes

  subroutine bufr_value(bufr_id, subset_id, var_name, value)

    integer, intent(in) :: bufr_id
    integer, intent(in) :: subset_id
    character(*), intent(in) :: var_name
    class(*), intent(out) :: value

    character(30) key
    integer key_defined, value_missing, ret

    write(key, "('#', I0, '#', A)") subset_id, var_name

    call codes_is_defined(bufr_id, key, key_defined, ret)
    if (ret /= CODES_SUCCESS) then
      write(*, *) '[Error]: Failed to query key ' // trim(key) // '!'
      stop 1
    end if
    if (key_defined == 1) then
      call codes_is_missing(bufr_id, key, value_missing, ret)
      if (ret /= CODES_SUCCESS) then
        write(*, *) '[Error]: Failed to query value of key ' // trim(key) // '!'
        stop 1
      end if
    end if
    ! FIXME: codes_is_missing does not provide correct information. All values are 1.
    if (key_defined == 1 .and. value_missing == 1) then
      select type (value)
      type is (integer)
        call codes_get(bufr_id, key, value)
      type is (real)
        call codes_get(bufr_id, key, value)
      type is (character(*))
        call codes_get(bufr_id, key, value)
      class default
        write(*, *) '[Error]: Unsupported BUFR value type!'
        stop 1
      end select
    else
      select type (value)
      type is (integer)
        value = int_missing_value
      type is (real)
        value = real_missing_value
      type is (character(*))
        value = str_missing_value
      class default
        write(*, *) '[Error]: Unsupported BUFR value type!'
        stop 1
      end select
    end if

  end subroutine bufr_value

end module utils_mod
