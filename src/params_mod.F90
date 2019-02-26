module params_mod

  implicit none

  real(8), parameter :: pi = atan(1.0d0) * 4.0d0
  real(8), parameter :: radian = pi / 180.0d0
  real(8), parameter :: degree = 1.0d0 / radian
  real, parameter :: freezing_point = 273.15
  real, parameter :: triple_point = 273.16
  real, parameter :: Rd = 287.058 ! J/(kg*K)
  real, parameter :: Rv = 461.495 ! J/(kg*K)

end module params_mod
