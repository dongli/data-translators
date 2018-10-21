module utils_mod

  use params_mod
  use eccodes

  implicit none

  interface resize_array
    module procedure resize_real4_array
  end interface resize_array

  interface unique_element_count
    module procedure unique_real4_element_count
  end interface unique_element_count

contains

  real function add(a, b) result(res)

    real, intent(in) :: a
    real, intent(in) :: b

    if (a /= real_missing_value) then
      res = a + b
    else
      res = real_missing_value
    end if

  end function add

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

  real function p_to_slp(p, T, z) result(slp)

    real, intent(in) :: p
    real, intent(in) :: T
    real, intent(in) :: z

    slp = p * (1.0 - 0.0065 * z / (T + 0.0065 * z + 273.15)) * (-5.257)

  end function p_to_slp

  subroutine prepbufr_raw(stack, value, stack_qc, stack_pc, qc)

    real(8), intent(in) :: stack(:)
    real, intent(out) :: value
    real(8), intent(in), optional :: stack_qc(:)
    real(8), intent(in), optional :: stack_pc(:)
    integer, intent(out), optional :: qc

    integer i

    value = real_missing_value
    if (present(qc)) qc = int_missing_value
    if (present(stack_qc) .and. .not. present(stack_pc)) then
      do i = 1, size(stack)
        if (stack_qc(i) /= 3 .and. stack_qc(i) /= 7) then
          value = stack(i)
          if (present(qc)) qc = stack_qc(i)
          exit
        end if
      end do
    else if (present(stack_qc) .and. present(stack_pc)) then
      do i = 1, size(stack)
        if (stack_pc(i) == 1) then
          value = stack(i)
          if (present(qc)) qc = stack_qc(i)
          exit
        else if (stack_pc(i) == missing_value_in_prepbufr) then
          exit
        end if
      end do
    else
      do i = 1, size(stack)
        if (stack(i) == missing_value_in_prepbufr) then
          if (i /= 1) value = stack(i-1)
          exit
        end if
      end do
    end if
    if (value == missing_value_in_prepbufr) value = real_missing_value

  end subroutine prepbufr_raw

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

  function prepbufr_stack(stack) result(res)

    real(8), intent(in) :: stack(:)
    real(8) res(size(stack))

    integer i

    do i = 1, size(stack)
      if (stack(i) == missing_value_in_prepbufr) then
        res(i) = real_missing_value
      else
        res(i) = stack(i)
      end if
    end do

  end function prepbufr_stack

  integer function prepbufr_value_count(stack) result(res)

    real(8), intent(in) :: stack(:)

    integer i

    res = 0
    do i = 1, size(stack)
      if (stack(i) == missing_value_in_prepbufr) then
        return
      end if
      res = res + 1
    end do

  end function prepbufr_value_count

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

  subroutine resize_real4_array(array, target_size)

    real(4), intent(inout), allocatable :: array(:)
    integer, intent(in) :: target_size

    real(4) buffer(target_size)

    buffer(:size(array)) = array(:)
    buffer(size(array)+1:) = real_missing_value
    deallocate(array)
    allocate(array(target_size))
    array(:) = buffer(:)

  end subroutine resize_real4_array

  integer function unique_real4_element_count(array1, array2) result(res)

    real(4), intent(in) :: array1(:)
    real(4), intent(in) :: array2(:)

    integer i, j

    res = size(array1) + size(array2)
    do i = 1, size(array1)
      do j = 1, size(array2)
        if (array1(i) == array2(j)) then
          res = res - 1
        end if
      end do
    end do

  end function unique_real4_element_count

end module utils_mod
