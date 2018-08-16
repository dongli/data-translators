module params_mod

  implicit none

  real(8), parameter :: pi = atan(1.0d0) * 4.0d0
  real(8), parameter :: radian = pi / 180.0d0
  real(8), parameter :: degree = 1.0d0 / radian

  ! Missing values
  character(3), parameter :: str_missing_value = 'N/A'
  integer, parameter :: int_missing_value = 99999
  real, parameter :: real_missing_value = 99999.0e0
  real(8), parameter :: missing_value_in_prepbufr = 99999997952.000000

end module params_mod