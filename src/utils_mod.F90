module utils_mod

  use params_mod
  use eccodes
  use string_mod

  implicit none

  interface is_missing
    module procedure is_missing_i4
    module procedure is_missing_r4
    module procedure is_missing_r8
  end interface is_missing

  interface multiply
    module procedure multiply_scalar
    module procedure multiply_array
  end interface multiply

  interface resize_array
    module procedure resize_real4_array
  end interface resize_array

  interface unique_element_count
    module procedure unique_real4_element_count
  end interface unique_element_count

  interface prepbufr_raw
    module procedure prepbufr_raw_i4
    module procedure prepbufr_raw_r8
  end interface prepbufr_raw

contains

  real function add(a, b) result(res)

    real, intent(in) :: a
    real, intent(in) :: b

    if (is_missing(a) .or. is_missing(b)) then
      res = real_missing_value
    else
      res = a + b
    end if

  end function add

  real function subtract(a, b) result(res)

    real, intent(in) :: a
    real, intent(in) :: b

    if (is_missing(a) .or. is_missing(b)) then
      res = real_missing_value
    else
      res = a - b
    end if

  end function subtract

  real function multiply_scalar(a, b) result(res)

    real, intent(in) :: a
    real, intent(in) :: b

    if (is_missing(a) .or. is_missing(b)) then
      res = real_missing_value
    else
      res = a * b
    end if

  end function multiply_scalar

  function multiply_array(a, b) result(res)

    real, intent(in) :: a(:)
    real, intent(in) :: b
    real res(size(a))

    integer i

    do i = 1, size(a)
      if (is_missing(a(i)) .or. is_missing(b)) then
        res(i) = real_missing_value
      else
        res(i) = a(i) * b
      end if
    end do

  end function multiply_array

  real function divide(a, b) result(res)

    real, intent(in) :: a
    real, intent(in) :: b

    if (is_missing(a) .or. is_missing(b)) then
      res = real_missing_value
    else
      res = a / b
    end if

  end function divide

  logical function is_missing_i4(x) result(res)

    integer(4), intent(in) :: x

    res = x == real_missing_value

  end function is_missing_i4

  logical function is_missing_r4(x) result(res)

    real(4), intent(in) :: x

    res = x == real_missing_value

  end function is_missing_r4

  logical function is_missing_r8(x) result(res)

    real(8), intent(in) :: x

    res = x == real_missing_value

  end function is_missing_r8

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

  real function sea_level_pressure(p, T, z) result(slp)

    real, intent(in) :: p
    real, intent(in) :: T
    real, intent(in) :: z

    if (is_missing(p) .or. is_missing(T) .or. is_missing(z)) then
      slp = real_missing_value
    else
      slp = p * (1.0 + 0.0065 * z / (T + freezing_point)) ** 5.257
    end if

  end function sea_level_pressure

  ! r  - mixing ratio
  ! sh - specific humidity
  ! rh - relative humidity

  real function vapor_pressure(p, r) result(e) ! Pa

    real, intent(in) :: p
    real, intent(in) :: r

    if (is_missing(p) .or. is_missing(r)) then
      e = real_missing_value
    else
      e = p * Rv * r / (Rd + Rv * r)
    end if

  end function vapor_pressure

  real function mixing_ratio(sh) result(r) ! 1

    real, intent(in) :: sh ! specific humidity (mg/kg)

    if (is_missing(sh)) then
      r = real_missing_value
    else
      r = sh / (1e6 - sh)
    end if

  end function mixing_ratio

  real function specific_humidity(r) result(sh) ! mg/kg

    real, intent(in) :: r ! mixing ratio (1)

    if (is_missing(r)) then
      sh = real_missing_value
    else
      sh = r / (1 + r) * 1e6
    end if

  end function specific_humidity

  real function relative_humidity(p, T, sh) result(rh) ! %

    real, intent(in) :: p  ! Pa
    real, intent(in) :: T  ! degC
    real, intent(in) :: sh ! Mg/Kg

    if (is_missing(p) .or. is_missing(T) .or. is_missing(sh)) then
      rh = real_missing_value
    else
      rh = vapor_pressure(p, mixing_ratio(sh)) / saturated_vapor_pressure(T) * 100
    end if

  end function relative_humidity

  real function saturated_vapor_pressure(T) result(esv)

    real, intent(in) :: T ! degC

    real, parameter :: T0 = triple_point
    real, parameter :: e0 = 611.0 ! Saturation vapor pressure at T0 (triple_point) (Pa)

    if (is_missing(T)) then
      esv = real_missing_value
    else
      esv = e0 * exp(17.67 * (T + freezing_point - T0) / (T + freezing_point - 29.25))
    end if

  end function saturated_vapor_pressure

  real function virtual_temperature(T, sh) result(Tv)

    real, intent(in) :: T
    real, intent(in) :: sh

    if (is_missing(T) .or. is_missing(sh)) then
      Tv = real_missing_value
    else
      Tv = (1.0 + 0.61 * sh) * T
    end if

  end function virtual_temperature

  real function dewpoint(p, sh) result(Td)

    real, intent(in) :: p
    real, intent(in) :: sh

    real, parameter :: T0 = triple_point
    real, parameter :: e0 = 611.0 ! Saturation vapor pressure at T0 (triple_point) (Pa)
    real a

    if (is_missing(p) .or. is_missing(sh)) then
      Td = real_missing_value
    else
      a = log(vapor_pressure(p, mixing_ratio(sh)) / e0) / 17.67
      Td = (29.25 * a - T0) / (a - 1) - freezing_point
    end if

  end function dewpoint

  real function knot_to_meter_per_second(value) result(res)

    real, intent(in) :: value

    if (value == real_missing_value) then
      res = real_missing_value
    else
      res = value * 1852.0 / 3600.0
    end if

  end function knot_to_meter_per_second

  subroutine prepbufr_raw_i4(stack, value, stack_qc, stack_pc, qc)

    real(8), intent(in) :: stack(:)
    integer(4), intent(out) :: value
    real(8), intent(in), optional :: stack_qc(:)
    real(8), intent(in), optional :: stack_pc(:)
    integer, intent(out), optional :: qc

    integer i

    value = int_missing_value
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
        if (stack_pc(i) == 1 .or. stack_qc(i) == 2) then
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
    if (value == missing_value_in_prepbufr) value = int_missing_value

  end subroutine prepbufr_raw_i4

  subroutine prepbufr_raw_r8(stack, value, stack_qc, stack_pc, qc)

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
        if (stack_pc(i) == 1 .and. (stack_qc(i) == 2 .or. stack_qc(i) == 6)) then
          ! Wind direction in GDAS is missing value when QC is 2, so we need to check if stack value is missing.
          ! If so, we use the previous one.
          if (stack(i) == missing_value_in_prepbufr) then
            if (i > 1) then
              value = stack(i-1)
            else
              value = real_missing_value
              ! write(*, *) '[Warning]: QC is ' // trim(to_string(stack_qc(i))) // ', but value is missing!'
            end if
          else
            value = stack(i)
          end if
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

  end subroutine prepbufr_raw_r8

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
    real res(size(stack))

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

  real function littler_value(value) result(res)

    real, intent(in) :: value

    res = merge(real_missing_value_in_littler, value, is_missing(value))

  end function littler_value

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
