module atm_formula_mod

  use params_mod
  use missing_value_mod

  implicit none

  private

  public wind_direction
  public wind_speed
  public wind_u_component
  public wind_v_component
  public knot_to_meter_per_second
  public wind_direction_code
  public virtual_temperature
  public dewpoint
  public dewpoint_from_relative_humidity
  public sea_level_pressure
  public vapor_pressure
  public saturated_vapor_pressure
  public mixing_ratio
  public mixing_ratio_from_relative_humidity
  public specific_humidity
  public specific_humidity_from_relative_humidity
  public specific_humidity_from_dewpoint
  public relative_humidity

  real(8), parameter :: Rd = 287.046 ! J kg-1 K-1
  real(8), parameter :: Rv = 461.497 ! J kg-1 K-1

  interface wind_direction
    module procedure wind_direction_r4
    module procedure wind_direction_r8
  end interface wind_direction

  interface wind_speed
    module procedure wind_speed_r4
    module procedure wind_speed_r8
  end interface wind_speed

  interface wind_u_component
    module procedure wind_u_component_r4
    module procedure wind_u_component_r8
  end interface wind_u_component

  interface wind_v_component
    module procedure wind_v_component_r4
    module procedure wind_v_component_r8
  end interface wind_v_component

  interface knot_to_meter_per_second
    module procedure knot_to_meter_per_second_r4
    module procedure knot_to_meter_per_second_r8
  end interface knot_to_meter_per_second

  interface wind_direction_code
    module procedure wind_direction_code_r4
    module procedure wind_direction_code_r8
  end interface wind_direction_code

  interface virtual_temperature
    module procedure virtual_temperature_r4
    module procedure virtual_temperature_r8
  end interface virtual_temperature

  interface dewpoint
    module procedure dewpoint_r4
    module procedure dewpoint_r8
  end interface dewpoint

  interface dewpoint_from_relative_humidity
    module procedure dewpoint_from_relative_humidity_r4
    module procedure dewpoint_from_relative_humidity_r8
  end interface dewpoint_from_relative_humidity

  interface sea_level_pressure
    module procedure sea_level_pressure_r4
    module procedure sea_level_pressure_r8
  end interface sea_level_pressure

  interface vapor_pressure
    module procedure vapor_pressure_r4
    module procedure vapor_pressure_r8
  end interface vapor_pressure

  interface saturated_vapor_pressure
    module procedure saturated_vapor_pressure_r4
    module procedure saturated_vapor_pressure_r8
  end interface saturated_vapor_pressure

  interface mixing_ratio
    module procedure mixing_ratio_r4
    module procedure mixing_ratio_r8
  end interface mixing_ratio

  interface mixing_ratio_from_relative_humidity
    module procedure mixing_ratio_from_relative_humidity_r4
    module procedure mixing_ratio_from_relative_humidity_r8
  end interface mixing_ratio_from_relative_humidity

  interface specific_humidity
    module procedure specific_humidity_r4
    module procedure specific_humidity_r8
  end interface specific_humidity

  interface specific_humidity_from_relative_humidity
    module procedure specific_humidity_from_relative_humidity_r4
    module procedure specific_humidity_from_relative_humidity_r8
  end interface specific_humidity_from_relative_humidity

  interface specific_humidity_from_dewpoint
    module procedure specific_humidity_from_dewpoint_r4
    module procedure specific_humidity_from_dewpoint_r8
  end interface specific_humidity_from_dewpoint

  interface relative_humidity
    module procedure relative_humidity_r4
    module procedure relative_humidity_r8
  end interface relative_humidity

contains

  ! ----------------------------------------------------------------------------
  !                              wind formula

  real(4) function wind_direction_r4(u, v) result(wd) ! deg

    real(4), intent(in) :: u ! m s-1
    real(4), intent(in) :: v ! m s-1

    real(4) angle

    if (is_missing(u) .or. is_missing(v)) then
      wd = real_missing_value
    else
      wd = atan2(u, v) * degree + 180
      if (wd >= 360) wd = wd - 360
      if (wd < 0) wd = wd + 360
    end if

  end function wind_direction_r4

  real(8) function wind_direction_r8(u, v) result(wd) ! deg

    real(8), intent(in) :: u ! m s-1
    real(8), intent(in) :: v ! m s-1

    real(8) angle

    if (is_missing(u) .or. is_missing(v)) then
      wd = real_missing_value
    else
      wd = atan2(u, v) * degree + 180
      if (wd >= 360) wd = wd - 360
      if (wd < 0) wd = wd + 360
    end if

  end function wind_direction_r8

  real(4) function wind_speed_r4(u, v) result(ws) ! m s-1

    real(4), intent(in) :: u ! m s-1
    real(4), intent(in) :: v ! m s-1

    if (is_missing(u) .or. is_missing(v)) then
      ws = real_missing_value
    else
      ws = sqrt(u**2 + v**2)
    end if

  end function wind_speed_r4

  real(8) function wind_speed_r8(u, v) result(ws) ! m s-1

    real(8), intent(in) :: u ! m s-1
    real(8), intent(in) :: v ! m s-1

    if (is_missing(u) .or. is_missing(v)) then
      ws = real_missing_value
    else
      ws = sqrt(u**2 + v**2)
    end if

  end function wind_speed_r8

  real(4) function wind_u_component_r4(ws, wd) result(u) ! m s-1

    real(4), intent(in) :: ws ! m s-1
    real(4), intent(in) :: wd ! deg

    if (is_missing(ws) .or. is_missing(wd)) then
      u = real_missing_value
    else
      u = - ws * sin(wd * radian)
    end if

  end function wind_u_component_r4

  real(8) function wind_u_component_r8(ws, wd) result(u) ! m s-1

    real(8), intent(in) :: ws ! m s-1
    real(8), intent(in) :: wd ! deg

    if (is_missing(ws) .or. is_missing(wd)) then
      u = real_missing_value
    else
      u = - ws * sin(wd * radian)
    end if

  end function wind_u_component_r8

  real(4) function wind_v_component_r4(ws, wd) result(v) ! m s-1

    real(4), intent(in) :: ws ! m s-1
    real(4), intent(in) :: wd ! deg

    if (is_missing(ws) .or. is_missing(wd)) then
      v = real_missing_value
    else
      v = - ws * cos(wd * radian)
    end if

  end function wind_v_component_r4

  real(8) function wind_v_component_r8(ws, wd) result(v) ! m s-1

    real(8), intent(in) :: ws ! m s-1
    real(8), intent(in) :: wd ! deg

    if (is_missing(ws) .or. is_missing(wd)) then
      v = real_missing_value
    else
      v = - ws * cos(wd * radian)
    end if

  end function wind_v_component_r8

  real(4) function knot_to_meter_per_second_r4(value) result(res)

    real(4), intent(in) :: value

    if (value == real_missing_value) then
      res = real_missing_value
    else
      res = value * 1852.0 / 3600.0
    end if

  end function knot_to_meter_per_second_r4

  real(8) function knot_to_meter_per_second_r8(value) result(res)

    real(8), intent(in) :: value

    if (value == real_missing_value) then
      res = real_missing_value
    else
      res = value * 1852.0d0 / 3600.0d0
    end if

  end function knot_to_meter_per_second_r8

  pure real(4) function wind_direction_code_r4(code) result(res)

    real(4), intent(in) :: code

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

  end function wind_direction_code_r4

  pure real(8) function wind_direction_code_r8(code) result(res)

    real(8), intent(in) :: code

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

  end function wind_direction_code_r8

  ! ----------------------------------------------------------------------------
  !                           temperature formula

  real(4) function virtual_temperature_r4(ta, sh) result(tv) ! degC

    real(4), intent(in) :: ta ! degC
    real(4), intent(in) :: sh ! mg kg-1

    if (is_missing(ta) .or. is_missing(sh)) then
      tv = real_missing_value
    else
      tv = (1.0 + 1e-3 * sh * (Rv - Rd) / Rd) * ta
    end if

  end function virtual_temperature_r4

  real(8) function virtual_temperature_r8(ta, sh) result(tv) ! degC

    real(8), intent(in) :: ta ! degC
    real(8), intent(in) :: sh ! mg kg-1

    if (is_missing(ta) .or. is_missing(sh)) then
      tv = real_missing_value
    else
      tv = (1.0d0 + 1e-3 * sh * (Rv - Rd) / Rd) * ta
    end if

  end function virtual_temperature_r8

  real(4) function dewpoint_r4(p, sh) result(td) ! degC

    real(4), intent(in) :: p  ! hPa
    real(4), intent(in) :: sh ! mg kg-1

    ! See (7) in Lawrence 2005 on BAMS.
    real(4), parameter :: A = 17.67
    real(4), parameter :: B = 243.5 ! degC
    real(4), parameter :: C = 6.112 ! hPa

    real(4) e ! hPa

    ! es(td) = e(ta)

    if (is_missing(p) .or. is_missing(sh) .or. sh == 0.0) then
      td = real_missing_value
    else
      e  = vapor_pressure(p, mixing_ratio(sh))
      td = B * log(e / C) / (A - log(e / C))
    end if

  end function dewpoint_r4

  real(8) function dewpoint_r8(p, sh) result(td) ! degC

    real(8), intent(in) :: p  ! hPa
    real(8), intent(in) :: sh ! mg kg-1

    ! See (7) in Lawrence 2005 on BAMS.
    real(8), parameter :: A = 17.67
    real(8), parameter :: B = 243.5 ! degC
    real(8), parameter :: C = 6.112 ! hPa

    real(8) e ! hPa

    ! es(td) = e(ta)

    if (is_missing(p) .or. is_missing(sh) .or. sh == 0.0) then
      td = real_missing_value
    else
      e  = vapor_pressure(p, mixing_ratio(sh))
      td = B * log(e / C) / (A - log(e / C))
    end if

  end function dewpoint_r8

  real(4) function dewpoint_from_relative_humidity_r4(ta, rh) result(td) ! degC

    real(4), intent(in) :: ta ! degC
    real(4), intent(in) :: rh ! %

    ! See (8) in Lawrence 2005 on BAMS.
    real(4), parameter :: A = 17.67
    real(4), parameter :: B = 243.5 ! degC

    real(4) tmp

    if (is_missing(ta) .or. is_missing(rh)) then
      td = real_missing_value
    else
      tmp = A * ta / (B + ta)
      td = B * (log(rh * 0.01) + tmp) / (A - log(rh * 0.01) - tmp)
    end if

  end function dewpoint_from_relative_humidity_r4

  real(8) function dewpoint_from_relative_humidity_r8(ta, rh) result(td) ! degC

    real(8), intent(in) :: ta ! degC
    real(8), intent(in) :: rh ! %

    ! See (8) in Lawrence 2005 on BAMS.
    real(8), parameter :: A = 17.67
    real(8), parameter :: B = 243.5 ! degC

    real(8) tmp

    if (is_missing(ta) .or. is_missing(rh)) then
      td = real_missing_value
    else
      tmp = A * ta / (B + ta)
      td = B * (log(rh * 0.01) + tmp) / (A - log(rh * 0.01) - tmp)
    end if

  end function dewpoint_from_relative_humidity_r8

  ! ----------------------------------------------------------------------------
  !                              pressure formula

  real(4) function sea_level_pressure_r4(p, ta, z) result(slp) ! hPa

    real(4), intent(in) :: p  ! hPa
    real(4), intent(in) :: ta ! degC
    real(4), intent(in) :: z  ! m

    if (is_missing(p) .or. is_missing(ta) .or. is_missing(z)) then
      slp = real_missing_value
    else
      slp = p * (1.0 + 0.0065 * z / (ta + freezing_point)) ** 5.257
    end if

  end function sea_level_pressure_r4

  real(8) function sea_level_pressure_r8(p, ta, z) result(slp) ! hPa

    real(8), intent(in) :: p  ! hPa
    real(8), intent(in) :: ta ! degC
    real(8), intent(in) :: z  ! m

    if (is_missing(p) .or. is_missing(ta) .or. is_missing(z)) then
      slp = real_missing_value
    else
      slp = p * (1.0 + 0.0065 * z / (ta + freezing_point)) ** 5.257
    end if

  end function sea_level_pressure_r8

  ! ----------------------------------------------------------------------------
  !                             moisture formula

  real(4) function vapor_pressure_r4(p, r) result(e) ! hPa

    real(4), intent(in) :: p ! hPa
    real(4), intent(in) :: r ! 1

    if (is_missing(p) .or. is_missing(r)) then
      e = real_missing_value
    else
      e = p * Rv * r / (Rd + Rv * r)
    end if

  end function vapor_pressure_r4

  real(8) function vapor_pressure_r8(p, r) result(e) ! hPa

    real(8), intent(in) :: p ! hPa
    real(8), intent(in) :: r ! 1

    if (is_missing(p) .or. is_missing(r)) then
      e = real_missing_value
    else
      e = p * Rv * r / (Rd + Rv * r)
    end if

  end function vapor_pressure_r8

  real(4) function saturated_vapor_pressure_r4(ta) result(es) ! hPa

    real(4), intent(in) :: ta ! degC

    ! See (6) in Lawrence 2005 on BAMS.
    real(4), parameter :: A = 17.67
    real(4), parameter :: B = 243.5 ! degC
    real(4), parameter :: C = 6.112 ! hPa

    if (is_missing(ta)) then
      es = real_missing_value
    else
      es = C * exp(A * ta / (B + ta))
    end if

  end function saturated_vapor_pressure_r4

  real(8) function saturated_vapor_pressure_r8(ta) result(es) ! hPa

    real(8), intent(in) :: ta ! degC

    ! See (6) in Lawrence 2005 on BAMS.
    real(8), parameter :: A = 17.67
    real(8), parameter :: B = 243.5 ! degC
    real(8), parameter :: C = 6.112 ! hPa

    if (is_missing(ta)) then
      es = real_missing_value
    else
      es = C * exp(A * ta / (B + ta))
    end if

  end function saturated_vapor_pressure_r8

  real(4) function mixing_ratio_r4(sh) result(r) ! 1

    real(4), intent(in) :: sh ! specific humidity (mg kg-1)

    if (is_missing(sh)) then
      r = real_missing_value
    else
      r = sh / (1e6 - sh)
    end if

  end function mixing_ratio_r4

  real(8) function mixing_ratio_r8(sh) result(r) ! 1

    real(8), intent(in) :: sh ! specific humidity (mg kg-1)

    if (is_missing(sh)) then
      r = real_missing_value
    else
      r = sh / (1e6 - sh)
    end if

  end function mixing_ratio_r8

  real(4) function mixing_ratio_from_relative_humidity_r4(p, ta, rh) result(r) ! 1

    real(4), intent(in) :: p  ! hPa
    real(4), intent(in) :: ta ! degC
    real(4), intent(in) :: rh ! %

    real(4) e, es ! hPa

    if (is_missing(p) .or. is_missing(ta) .or. is_missing(rh)) then
      r = real_missing_value
    else
      es = saturated_vapor_pressure(ta)
      e  = rh / 100.0 * es
      r = Rd / Rv / (p / e - 1)
    end if

  end function mixing_ratio_from_relative_humidity_r4

  real(8) function mixing_ratio_from_relative_humidity_r8(p, ta, rh) result(r) ! 1

    real(8), intent(in) :: p  ! hPa
    real(8), intent(in) :: ta ! degC
    real(8), intent(in) :: rh ! %

    real(8) e, es ! hPa

    if (is_missing(p) .or. is_missing(ta) .or. is_missing(rh)) then
      r = real_missing_value
    else
      es = saturated_vapor_pressure(ta)
      e  = rh / 100.0 * es
      r = Rd / Rv / (p / e - 1)
    end if

  end function mixing_ratio_from_relative_humidity_r8

  real(4) function specific_humidity_r4(r) result(sh) ! Kg Kg-1

    real(4), intent(in) :: r ! mixing ratio (1)

    if (is_missing(r)) then
      sh = real_missing_value
    else
      sh = r / (1 + r)
    end if

  end function specific_humidity_r4

  real(8) function specific_humidity_r8(r) result(sh) ! Kg Kg-1

    real(8), intent(in) :: r ! mixing ratio (1)

    if (is_missing(r)) then
      sh = real_missing_value
    else
      sh = r / (1 + r)
    end if

  end function specific_humidity_r8

  real(4) function specific_humidity_from_relative_humidity_r4(p, ta, rh) result(sh) ! Kg Kg-1

    real(4), intent(in) :: p  ! hPa
    real(4), intent(in) :: ta ! degC
    real(4), intent(in) :: rh ! %

    real(4) r

    if (is_missing(p) .or. is_missing(ta) .or. is_missing(rh)) then
      sh = real_missing_value
    else
      r  = mixing_ratio_from_relative_humidity(p, ta, rh)
      sh = specific_humidity(r)
    end if

  end function specific_humidity_from_relative_humidity_r4

  real(8) function specific_humidity_from_relative_humidity_r8(p, ta, rh) result(sh) ! Kg Kg-1

    real(8), intent(in) :: p  ! hPa
    real(8), intent(in) :: ta ! degC
    real(8), intent(in) :: rh ! %

    real(8) r

    if (is_missing(p) .or. is_missing(ta) .or. is_missing(rh)) then
      sh = real_missing_value
    else
      r  = mixing_ratio_from_relative_humidity(p, ta, rh)
      sh = specific_humidity(r)
    end if

  end function specific_humidity_from_relative_humidity_r8

  real(4) function specific_humidity_from_dewpoint_r4(p, ta, td) result(sh) ! Kg Kg-1

    real(4), intent(in) :: p  ! hPa
    real(4), intent(in) :: ta ! degC
    real(4), intent(in) :: td ! degC

    ! See (7) in Lawrence 2005 on BAMS.
    real(4), parameter :: A = 17.67
    real(4), parameter :: B = 243.5 ! degC
    real(4), parameter :: C = 6.112 ! hPa

    real(4) e, r

    if (is_missing(p) .or. is_missing(ta) .or. is_missing(td)) then
      sh = real_missing_value
    else
      e = C * exp(A * td / (B + td))
      r = Rd * e / Rv / (p - e)
      sh = specific_humidity(r)
    end if

  end function specific_humidity_from_dewpoint_r4

  real(8) function specific_humidity_from_dewpoint_r8(p, ta, td) result(sh) ! Kg Kg-1

    real(8), intent(in) :: p  ! hPa
    real(8), intent(in) :: ta ! degC
    real(8), intent(in) :: td ! degC

    ! See (7) in Lawrence 2005 on BAMS.
    real(8), parameter :: A = 17.67
    real(8), parameter :: B = 243.5 ! degC
    real(8), parameter :: C = 6.112 ! hPa

    real(8) e, r

    if (is_missing(p) .or. is_missing(ta) .or. is_missing(td)) then
      sh = real_missing_value
    else
      e = C * exp(A * td / (B + td))
      r = Rd * e / Rv / (p - e)
      sh = specific_humidity(r)
    end if

  end function specific_humidity_from_dewpoint_r8

  real(4) function relative_humidity_r4(p, ta, sh) result(rh) ! %

    real(4), intent(in) :: p  ! Pa
    real(4), intent(in) :: ta ! degC
    real(4), intent(in) :: sh ! Kg Kg-1

    if (is_missing(p) .or. is_missing(ta) .or. is_missing(sh)) then
      rh = real_missing_value
    else
      rh = vapor_pressure(p, mixing_ratio(sh)) / saturated_vapor_pressure(ta) * 100
    end if

  end function relative_humidity_r4

  real(8) function relative_humidity_r8(p, ta, sh) result(rh) ! %

    real(8), intent(in) :: p  ! Pa
    real(8), intent(in) :: ta ! degC
    real(8), intent(in) :: sh ! Kg Kg-1

    if (is_missing(p) .or. is_missing(ta) .or. is_missing(sh)) then
      rh = real_missing_value
    else
      rh = vapor_pressure(p, mixing_ratio(sh)) / saturated_vapor_pressure(ta) * 100
    end if

  end function relative_humidity_r8

end module atm_formula_mod
