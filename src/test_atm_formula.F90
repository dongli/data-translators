program atm_formula_test

  use unit_test
  use atm_formula_mod

  implicit none

  real p, e, es, T, Td, sh, rh, r

  call test_suite_init('Test atm_formula_mod module')

  p = 762.0e2
  T = -12.5
  sh = 817.0 ! mg/kg

  r = mixing_ratio(sh)
  call assert_approximate(r, 0.0008176680347844189, __FILE__, __LINE__)
  call assert_approximate(specific_humidity(r), sh, __FILE__, __LINE__)

  e = vapor_pressure(p, r)
  call assert_approximate(e, 100.04259626350864, __FILE__, __LINE__)

  es = saturated_vapor_pressure(T)
  call assert_approximate(es, 234.9224375494164, __FILE__, __LINE__)

  rh = relative_humidity(p, T, sh)
  call assert_approximate(rh, 42.509894518278585, __FILE__, __LINE__, eps=1e-2)

  p = 891.0e2
  T = -9.9
  sh = 2014.0

  r = mixing_ratio(sh)
  call assert_approximate(r, 0.002018064381664673, __FILE__, __LINE__)
  call assert_approximate(specific_humidity(r), sh, __FILE__, __LINE__)

  e = vapor_pressure(p, r)
  call assert_approximate(e, 288.1571625261647, __FILE__, __LINE__)

  es = saturated_vapor_pressure(T)
  call assert_approximate(es, 289.04061579025395, __FILE__, __LINE__)

  rh = relative_humidity(p, T, sh)
  call assert_approximate(rh, 99.69335807814043, __FILE__, __LINE__)

  Td = dewpoint(p, sh)
  call assert_approximate(Td, -9.938943591342465, __FILE__, __LINE__)

  sh = specific_humidity_from_dewpoint(p, T, Td)
  call assert_approximate(sh, 2014.0, __FILE__, __LINE__)

  call test_case_report('Test atm_formula_mod')

  call test_suite_final()

end program atm_formula_test
