module atm_formula_mod

  use params_mod
  use missing_value_mod

  implicit none

  real, parameter :: Rd = 287.046 ! J kg-1 K-1
  real, parameter :: Rv = 461.497 ! J kg-1 K-1

contains

  ! ----------------------------------------------------------------------------
  !                              wind formula

  real function wind_direction(u, v) result(wd) ! deg

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
    real, intent(in) :: wd ! deg

    if (is_missing(ws) .or. is_missing(wd)) then
      u = real_missing_value
    else
      u = - ws * sin(wd * radian)
    end if

  end function wind_u_component

  real function wind_v_component(ws, wd) result(v) ! m s-1

    real, intent(in) :: ws ! m s-1
    real, intent(in) :: wd ! deg

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

  pure real function wind_direction_code(code) result(res)

    real, intent(in) :: code

    select case (int(code))
    case (1)
      res = 0.0
    case (2)
      res = 22.5
    case (3)
      res = 45.0
    case (4)
      res = 67.5
    case (5)
      res = 90.0
    case (6)
      res = 112.5
    case (7)
      res = 135.0
    case (8)
      res = 157.5
    case (9)
      res = 180.0
    case (10)
      res = 202.5
    case (11)
      res = 225.0
    case (12)
      res = 247.5
    case (13)
      res = 270.0
    case (14)
      res = 292.5
    case (15)
      res = 315.0
    case (16)
      res = 337.5
    case (17)
      res = real_missing_value
    end select

  end function wind_direction_code

  ! ----------------------------------------------------------------------------
  !                           temperature formula

  real function virtual_temperature(ta, sh) result(tv) ! degC

    real, intent(in) :: ta ! degC
    real, intent(in) :: sh ! mg kg-1

    if (is_missing(ta) .or. is_missing(sh)) then
      tv = real_missing_value
    else
      tv = (1.0 + 1e-3 * sh * (Rv - Rd) / Rd) * ta
    end if

  end function virtual_temperature

  real function dewpoint(p, sh) result(td) ! degC

    real, intent(in) :: p  ! hPa
    real, intent(in) :: sh ! mg kg-1

    ! See (7) in Lawrence 2005 on BAMS.
    real, parameter :: A = 17.67
    real, parameter :: B = 243.5 ! degC
    real, parameter :: C = 6.112 ! hPa

    real e ! hPa

    ! es(td) = e(ta)

    if (is_missing(p) .or. is_missing(sh)) then
      td = real_missing_value
    else
      e  = vapor_pressure(p, mixing_ratio(sh))
      td = B * log(e / C) / (A - log(e / C))
    end if

  end function dewpoint

  real function dewpoint_from_relative_humidity(ta, rh) result(td) ! degC

    real, intent(in) :: ta ! degC
    real, intent(in) :: rh ! %

    ! See (8) in Lawrence 2005 on BAMS.
    real, parameter :: A = 17.67
    real, parameter :: B = 243.5 ! degC

    real tmp

    if (is_missing(ta) .or. is_missing(rh)) then
      td = real_missing_value
    else
      tmp = A * ta / (B + ta)
      td = B * (log(rh * 0.01) + tmp) / (A - log(rh * 0.01) - tmp)
    end if

  end function dewpoint_from_relative_humidity

  ! ----------------------------------------------------------------------------
  !                              pressure formula

  real function sea_level_pressure(p, ta, z) result(slp) ! hPa

    real, intent(in) :: p  ! hPa
    real, intent(in) :: ta ! degC
    real, intent(in) :: z  ! m

    if (is_missing(p) .or. is_missing(ta) .or. is_missing(z)) then
      slp = real_missing_value
    else
      slp = p * (1.0 + 0.0065 * z / (ta + freezing_point)) ** 5.257
    end if

  end function sea_level_pressure

  ! ----------------------------------------------------------------------------
  !                             moisture formula

  real function vapor_pressure(p, r) result(e) ! hPa

    real, intent(in) :: p ! hPa
    real, intent(in) :: r ! 1

    if (is_missing(p) .or. is_missing(r)) then
      e = real_missing_value
    else
      e = p * Rv * r / (Rd + Rv * r)
    end if

  end function vapor_pressure

  real function saturated_vapor_pressure(ta) result(es) ! hPa

    real, intent(in) :: ta ! degC

    ! See (6) in Lawrence 2005 on BAMS.
    real, parameter :: A = 17.67
    real, parameter :: B = 243.5 ! degC
    real, parameter :: C = 6.112 ! hPa

    if (is_missing(ta)) then
      es = real_missing_value
    else
      es = C * exp(A * ta / (B + ta))
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

  real function mixing_ratio_from_relative_humidity(p, ta, rh) result(r) ! 1

    real, intent(in) :: p  ! hPa
    real, intent(in) :: ta ! degC
    real, intent(in) :: rh ! %

    real e, es ! hPa

    if (is_missing(p) .or. is_missing(ta) .or. is_missing(rh)) then
      r = real_missing_value
    else
      es = saturated_vapor_pressure(ta)
      e  = rh / 100.0 * es
      r = Rd / Rv / (p / e - 1)
    end if

  end function mixing_ratio_from_relative_humidity

  real function specific_humidity(r) result(sh) ! mg kg-1

    real, intent(in) :: r ! mixing ratio (1)

    if (is_missing(r)) then
      sh = real_missing_value
    else
      sh = r / (1 + r) * 1e6
    end if

  end function specific_humidity

  real function specific_humidity_from_relative_humidity(p, ta, rh) result(sh) ! mg kg-1

    real, intent(in) :: p  ! hPa
    real, intent(in) :: ta ! degC
    real, intent(in) :: rh ! %

    real r

    if (is_missing(p) .or. is_missing(ta) .or. is_missing(rh)) then
      sh = real_missing_value
    else
      r  = mixing_ratio_from_relative_humidity(p, ta, rh)
      sh = specific_humidity(r)
    end if

  end function specific_humidity_from_relative_humidity

  real function specific_humidity_from_dewpoint(p, ta, td) result(sh) ! mg kg-1

    real, intent(in) :: p  ! hPa
    real, intent(in) :: ta ! degC
    real, intent(in) :: td ! degC

    ! See (7) in Lawrence 2005 on BAMS.
    real, parameter :: A = 17.67
    real, parameter :: B = 243.5 ! degC
    real, parameter :: C = 6.112 ! hPa

    real e, r

    if (is_missing(p) .or. is_missing(ta) .or. is_missing(td)) then
      sh = real_missing_value
    else
      e = C * exp(A * td / (B + td))
      r = Rd * e / Rv / (p - e)
      sh = specific_humidity(r)
    end if

  end function specific_humidity_from_dewpoint

  real function relative_humidity(p, ta, sh) result(rh) ! %

    real, intent(in) :: p  ! Pa
    real, intent(in) :: ta ! degC
    real, intent(in) :: sh ! Mg/Kg

    if (is_missing(p) .or. is_missing(ta) .or. is_missing(sh)) then
      rh = real_missing_value
    else
      rh = vapor_pressure(p, mixing_ratio(sh)) / saturated_vapor_pressure(ta) * 100
    end if

  end function relative_humidity

end module atm_formula_mod
