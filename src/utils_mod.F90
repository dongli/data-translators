module utils_mod

  use params_mod
  use hash_table_mod
  use synop_mod
#ifdef HAS_LIB_ECCODES
  use eccodes
#endif
#ifdef HAS_LIB_NETCDF
  use netcdf
#endif
#ifdef HAS_LIB_ODB_API
  use odbql_wrappers
#endif
  use string
  use missing_value_mod
  use atm_formula_mod

  implicit none

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

  real function subtract_wind_direction(a, b) result(res)

    real, intent(in) :: a
    real, intent(in) :: b

    if (is_missing(a) .or. is_missing(b)) then
      res = real_missing_value
    else if (a >= b) then
      res = a - b
      if (res > 0 .and. 360 - res < res) then
        res = res - 360
      else if (res < 0) then
        res = res + 360
      end if
    end if

  end function subtract_wind_direction

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

  subroutine prepbufr_raw_i4(stack, value, stack_qc, stack_pc, qc)

    real(8), intent(in) :: stack(:)
    integer(4), intent(out) :: value
    real(8), intent(in), optional :: stack_qc(:)
    real(8), intent(in), optional :: stack_pc(:)
    integer, intent(out), optional :: qc

    integer i, j

    ! Just return the first value.
    value = stack(1)
    if (present(qc)) qc = stack_qc(1)
    if (value == missing_value_in_prepbufr) value = int_missing_value
    if (qc == missing_value_in_prepbufr) qc = int_missing_value

  end subroutine prepbufr_raw_i4

  subroutine prepbufr_raw_r8(stack, value, stack_qc, stack_pc, qc)

    real(8), intent(in) :: stack(:)
    real, intent(out) :: value
    real(8), intent(in), optional :: stack_qc(:)
    real(8), intent(in), optional :: stack_pc(:)
    integer, intent(out), optional :: qc

    integer i, j

    if (present(qc)) qc = int_missing_value
    if (present(stack_qc) .and. .not. present(stack_pc)) then
      ! Just return the first value.
      value = stack(1)
      if (present(qc)) qc = stack_qc(1)
    else if (present(stack_qc) .and. present(stack_pc)) then
      do i = 1, size(stack)
        if (stack_pc(i) == 8) then ! Virtual temprature process
          cycle
        else
          value = stack(i)
          if (present(qc)) qc = stack_qc(i)
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
    if (present(qc) .and. qc == missing_value_in_prepbufr) qc = int_missing_value

  end subroutine prepbufr_raw_r8

  function prepbufr_codes(codes) result(res)

    real(8), intent(in) :: codes(:)
    integer res(size(codes))

    integer i

    do i = 1, size(codes)
      if (is_missing(codes(i), src='prepbufr')) then
        res(i) = int_missing_value
      else
        res(i) = int(codes(i))
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

  real function prepbufr_correct(stack, stack_qc, stack_pc) result(res)

    real(8), intent(in) :: stack(:)
    real(8), intent(in) :: stack_qc(:)
    real(8), intent(in) :: stack_pc(:)

    integer n, i

    n = prepbufr_value_count(stack)
    if (n == 0) then
      res = real_missing_value
      return
    end if
    ! Skip virtual temperature change.
    i = merge(2, 1, stack_pc(1) == 8)
    res = stack(i) - stack(n)

  end function prepbufr_correct

#ifdef HAS_LIB_ECCODES
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
#endif

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

#ifdef HAS_LIB_NETCDF
  subroutine handle_netcdf_error(ierr, file, line)

    integer, intent(in) :: ierr
    character(*), intent(in), optional :: file
    integer, intent(in), optional :: line

    if (ierr /= nf90_noerr) then
      if (present(file) .and. present(line)) then
        write(*, *) '[Error]: ' // trim(file) // ':' // trim(to_string(line)) // ': ' // trim(nf90_strerror(ierr)) // '!'
      else
        write(*, *) '[Error]: ' // trim(nf90_strerror(ierr)) // '!'
      end if
      stop 1
    end if

  end subroutine handle_netcdf_error
#endif

#ifdef HAS_LIB_ODB_API
  function odb_values_placeholder(n) result(res)

    integer, intent(in) :: n
    character(:), allocatable :: res

    character(n * 2 - 1) tmp

    integer i

    do i = 1, n - 1
      tmp(2*i-1:2*i) = '?,'
    end do
    tmp(2*n-1:2*n-1) = '?'
    res = tmp

  end function odb_values_placeholder

  subroutine odb_all_bind_null(odb_stmt, n)

    type(odbql_stmt), intent(in) :: odb_stmt
    integer, intent(in) :: n

    integer i

    do i = 1, n
      call odbql_bind_null(odb_stmt, i)
    end do

  end subroutine odb_all_bind_null
#endif

  subroutine load_cma_synop_stations(stations)

    type(hash_table_type), intent(inout) :: stations

    integer iostat
    character(10) platform_id
    real lon, lat, z
    type(synop_station_type), pointer :: station

    open(10, file=dirname(__FILE__) // '/../notes/cma_synop_stations.txt', status='old')
    do while (.true.)
      read(10, *, iostat=iostat) platform_id, lon, lat, z
      if (iostat /= 0) exit
      if (stations%hashed(platform_id)) then
        write(*, *) '[Error]: Duplicate SYNOP station ', trim(platform_id), '!'
        stop 1
      else
        allocate(station)
        station%name = platform_id
        call station%init(platform_id, lon, lat, z)
        station%seq_id = stations%size
        call stations%insert(platform_id, station)
      end if
    end do
    close(10)

    write(*, *) '[Notice]: Load ' // trim(to_string(stations%size)) // ' SYNOP stations.'

  end subroutine load_cma_synop_stations

end module utils_mod
