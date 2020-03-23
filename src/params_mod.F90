module params_mod

  implicit none

  real(8), parameter :: pi = atan(1.0d0) * 4.0d0
  real(8), parameter :: radian = pi / 180.0d0
  real(8), parameter :: degree = 1.0d0 / radian
  real, parameter :: freezing_point = 273.15
  real, parameter :: triple_point = 273.16

end module params_mod
