module missing_value_mod

  implicit none

  private

  public is_missing  
  public str_missing_value
  public int_missing_value
  public real_missing_value
  public missing_value_in_prepbufr
  public real_missing_value_in_littler
  public int_missing_value_in_littler

  character(3), parameter :: str_missing_value = 'N/A'
  integer, parameter :: int_missing_value = -100000000
  real, parameter :: real_missing_value = -1.0e10
  real(8), parameter :: missing_value_in_prepbufr = 99999997952.000000
  real, parameter :: real_missing_value_in_littler = -888888.0
  integer, parameter :: int_missing_value_in_littler = -888888
  real, parameter :: real_missing_value_in_cimiss_1 = 999999.0
  real, parameter :: real_missing_value_in_cimiss_2 = 999998.0
  real, parameter :: real_missing_value_in_cimiss_3 = 999997.0
  integer, parameter :: int_missing_value_in_cimiss_1 = 999999
  integer, parameter :: int_missing_value_in_cimiss_2 = 999998
  integer, parameter :: int_missing_value_in_cimiss_3 = 999997

  interface is_missing
    module procedure is_missing_str
    module procedure is_missing_i4
    module procedure is_missing_r4
    module procedure is_missing_r8
  end interface is_missing

contains

  logical function is_missing_str(x) result(res)

    character(*), intent(in) :: x

    res = x == str_missing_value

  end function is_missing_str

  logical function is_missing_i4(x, src) result(res)

    integer, intent(in) :: x
    character(*), intent(in), optional :: src

    if (present(src)) then
      select case (src)
      case ('cimiss')
        res = x == int_missing_value_in_cimiss_1 .or. x == int_missing_value_in_cimiss_2 .or. x == int_missing_value_in_cimiss_3
      case default
        write(*, *) '[Error]: is_missing: Unknown src ' // trim(src) // '!'
      end select
    else
      res = x == int_missing_value
    end if

  end function is_missing_i4

  logical function is_missing_r4(x, src) result(res)

    real(4), intent(in) :: x
    character(*), intent(in), optional :: src

    if (present(src)) then
      select case (src)
      case ('cimiss')
        res = x == real_missing_value_in_cimiss_1 .or. x == real_missing_value_in_cimiss_2 .or. x == real_missing_value_in_cimiss_3
      case ('prepbufr')
        res = x == missing_value_in_prepbufr
      case default
        write(*, *) '[Error]: is_missing: Unknown src ' // trim(src) // '!'
      end select
    else
      res = x == real_missing_value
    end if

  end function is_missing_r4

  logical function is_missing_r8(x, src) result(res)

    real(8), intent(in) :: x
    character(*), intent(in), optional :: src

    if (present(src)) then
      select case (src)
      case ('cimiss')
        res = x == real_missing_value_in_cimiss_1 .or. x == real_missing_value_in_cimiss_2
      case ('prepbufr')
        res = x == missing_value_in_prepbufr
      case default
        write(*, *) '[Error]: is_missing: Unknown src ' // trim(src) // '!'
      end select
    else
      res = x == real_missing_value
    end if

  end function is_missing_r8

end module missing_value_mod
