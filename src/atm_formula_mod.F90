module atm_formula_mod

  use params_mod
  use missing_value_mod

  implicit none

contains

  ! ----------------------------------------------------------------------------
  !                              wind formula

  real function wind_direction(u, v) result(wd) ! degree

    real, intent(in) :: u ! m s-1
    real, intent(in) :: v ! m s-1

    real angle

    if (is_missing(u) .or. is_missing(v)) then
      wd = real_missing_value
    else
      wd = atan2(u, v) * degree + 180.0
      if (wd >= 360) wd = wd - 360.0d0
      if (wd < 0) wd = wd + 360.0d0
    end if

  end function wind_direction

  real function wind_speed(u, v) result(ws) ! m s-1

    real, intent(in) :: u ! m s-1
    real, intent(in) :: v ! m s-1

    if (is_missing(u) .or. is_missing(v)) then
      ws = real_missing_value
    else
      ws = sqrt(u**2 + v**2)
    end if

  end function wind_speed

  real function wind_u_component(ws, wd) result(u) ! m s-1

    real, intent(in) :: ws ! m s-1
    real, intent(in) :: wd ! degree

    if (is_missing(ws) .or. is_missing(wd)) then
      u = real_missing_value
    else
      u = - ws * sin(wd * radian)
    end if

  end function wind_u_component

  real function wind_v_component(ws, wd) result(v) ! m s-1

    real, intent(in) :: ws ! m s-1
    real, intent(in) :: wd ! degree

    if (is_missing(ws) .or. is_missing(wd)) then
      v = real_missing_value
    else
      v = - ws * cos(wd * radian)
    end if

  end function wind_v_component

  real function knot_to_meter_per_second(value) result(res)

    real, intent(in) :: value

    if (value == real_missing_value) then
      res = real_missing_value
    else
      res = value * 1852.0 / 3600.0
    end if

  end function knot_to_meter_per_second

  ! ----------------------------------------------------------------------------
  !                           temperature formula

  real function virtual_temperature(T, sh) result(Tv) ! degC

    real, intent(in) :: T  ! degC
    real, intent(in) :: sh ! mg kg-1

    if (is_missing(T) .or. is_missing(sh)) then
      Tv = real_missing_value
    else
      Tv = (1.0 + 0.608e-3 * sh) * T
    end if

  end function virtual_temperature

  real function dewpoint(p, sh) result(Td) ! degC

    real, intent(in) :: p  ! Pa
    real, intent(in) :: sh ! mg kg-1

    ! See (7) in Lawrence 2005 on BAMS.
    real, parameter :: A = 17.67
    real, parameter :: B = 243.5 ! degC
    real, parameter :: C = 611.2 ! Pa

    real e

    ! es(Td) = e(T)

    if (is_missing(p) .or. is_missing(sh)) then
      Td = real_missing_value
    else
      e  = vapor_pressure(p, mixing_ratio(sh))
      Td = B * log(e / C) / (A - log(e / C))
    end if

  end function dewpoint

  real function dewpoint_from_relative_humidity(T, rh) result(Td) ! degC

    real, intent(in) :: T  ! degC
    real, intent(in) :: rh ! %

    ! See (8) in Lawrence 2005 on BAMS.
    real, parameter :: A = 17.67
    real, parameter :: B = 243.5 ! degC
    real, parameter :: C = 611.2 ! Pa

    real tmp

    if (is_missing(T) .or. is_missing(rh)) then
      Td = real_missing_value
    else
      tmp = A * T / (B + T)
      Td = B * (log(rh * 0.01) + tmp) / (A - log(rh * 0.01) - tmp)
    end if

  end function dewpoint_from_relative_humidity

  ! ----------------------------------------------------------------------------
  !                              pressure formula

  real function sea_level_pressure(p, T, z) result(slp) ! Pa

    real, intent(in) :: p ! Pa
    real, intent(in) :: T ! degC
    real, intent(in) :: z ! m

    if (is_missing(p) .or. is_missing(T) .or. is_missing(z)) then
      slp = real_missing_value
    else
      slp = p * (1.0 + 0.0065 * z / (T + freezing_point)) ** 5.257
    end if

  end function sea_level_pressure

  ! ----------------------------------------------------------------------------
  !                             moisture formula

  real function vapor_pressure(p, r) result(e) ! Pa

    real, intent(in) :: p ! Pa
    real, intent(in) :: r ! 1

    if (is_missing(p) .or. is_missing(r)) then
      e = real_missing_value
    else
      e = p * Rv * r / (Rd + Rv * r)
    end if

  end function vapor_pressure

  real function saturated_vapor_pressure(T) result(es) ! Pa

    real, intent(in) :: T ! degC

    ! See (6) in Lawrence 2005 on BAMS.
    real, parameter :: A = 17.67
    real, parameter :: B = 243.5 ! degC
    real, parameter :: C = 611.2 ! Pa

    if (is_missing(T)) then
      es = real_missing_value
    else
      es = C * exp(A * T / (B + T))
    end if

  end function saturated_vapor_pressure

  real function mixing_ratio(sh) result(r) ! 1

    real, intent(in) :: sh ! specific humidity (mg kg-1)

    if (is_missing(sh)) then
      r = real_missing_value
    else
      r = sh / (1e6 - sh)
    end if

  end function mixing_ratio

  real function mixing_ratio_from_relative_humidity(p, T, rh) result(r) ! 1

    real, intent(in) :: p  ! Pa
    real, intent(in) :: T  ! degC
    real, intent(in) :: rh ! %

    real e, es

    if (is_missing(p) .or. is_missing(T) .or. is_missing(rh)) then
      r = real_missing_value
    else
      es = saturated_vapor_pressure(T)
      e  = rh / 100.0 * es
      r = Rd / Rv / (p / e - 1)
    end if

  end function mixing_ratio_from_relative_humidity

  real function specific_humidity(r) result(sh) ! mg/kg

    real, intent(in) :: r ! mixing ratio (1)

    if (is_missing(r)) then
      sh = real_missing_value
    else
      sh = r / (1 + r) * 1e6
    end if

  end function specific_humidity

  real function specific_humidity_from_relative_humidity(p, T, rh) result(sh) ! mg/kg

    real, intent(in) :: p  ! Pa
    real, intent(in) :: T  ! degC
    real, intent(in) :: rh ! %

    real r

    if (is_missing(p) .or. is_missing(T) .or. is_missing(rh)) then
      sh = real_missing_value
    else
      r  = mixing_ratio_from_relative_humidity(p, T, rh)
      sh = specific_humidity(r)
    end if

  end function specific_humidity_from_relative_humidity

  real function specific_humidity_from_dewpoint(p, T, Td) result(sh) ! mg/kg

    real, intent(in) :: p  ! Pa
    real, intent(in) :: T  ! degC
    real, intent(in) :: Td ! degC

    ! See (7) in Lawrence 2005 on BAMS.
    real, parameter :: A = 17.67
    real, parameter :: B = 243.5 ! degC
    real, parameter :: C = 611.2 ! Pa

    real e, r

    if (is_missing(p) .or. is_missing(T) .or. is_missing(Td)) then
      sh = real_missing_value
    else
      e = C * exp(A * Td / (B + Td))
      r = Rd * e / Rv / (p - e)
      sh = specific_humidity(r)
    end if

  end function specific_humidity_from_dewpoint

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

end module atm_formula_mod
