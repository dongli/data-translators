program decoder_test

  use unit_test
  use linked_list_mod
  use synop_mod
  use synop_prepbufr_mod
  use metar_prepbufr_mod
  use amdar_bufr_mod
  use amdar_prepbufr_mod
  use raob_prepbufr_mod

  implicit none

  type(linked_list_iterator_type) record_iterator

  call test_case_init()

  call test_case_create('Test SYNOP decoder')

  call synop_prepbufr_decode('../sample-data/gdas-prepbufr/prepqc.gdas.2015120100')

  record_iterator = linked_list_iterator(records)
  do while (.not. record_iterator%ended())
    select type (record => record_iterator%value)
    type is (synop_record_type)
      if (record%station%name == '03220') then
        call assert_equal(record%sfc_temperature,                 4.0,                      __FILE__, __LINE__)
        call assert_equal(record%sfc_temperature_stack(:3),       [4.69999981, 4.0, 4.0],   __FILE__, __LINE__)
        call assert_equal(record%sfc_temperature_qc(:3),          [9, 9, 2],                __FILE__, __LINE__)
        call assert_equal(record%sfc_temperature_pc(:3),          [8, 4, 1],                __FILE__, __LINE__)
        call assert_equal(record%sfc_specific_humidity,           4352.0,                   __FILE__, __LINE__)
        call assert_equal(record%sfc_specific_humidity_stack(:3), [4352.0, 4352.0, 4352.0], __FILE__, __LINE__)
        call assert_equal(record%sfc_specific_humidity_qc(:3),    [9, 9, 2],                __FILE__, __LINE__)
        call assert_equal(record%sfc_specific_humidity_pc(:3),    [8, 4, 1],                __FILE__, __LINE__)
        call assert_equal(record%sfc_dewpoint,                    2.0,                      __FILE__, __LINE__)
        call assert_equal(record%sfc_pressure,                    1011.09998,               __FILE__, __LINE__)
        call assert_equal(record%sfc_pressure_stack(:1),          [1011.09998],             __FILE__, __LINE__)
        call assert_equal(record%sfc_pressure_qc(:1),             [2],                      __FILE__, __LINE__)
        call assert_equal(record%sfc_pressure_pc(:1),             [1],                      __FILE__, __LINE__)
        call assert_equal(record%sfc_wind_speed,                  real_missing_value,       __FILE__, __LINE__)
        call assert_equal(record%sfc_wind_direction,              real_missing_value,       __FILE__, __LINE__)
        call assert_equal(record%sfc_wind_u_stack(1),             real_missing_value,       __FILE__, __LINE__)
        call assert_equal(record%sfc_wind_v_stack(1),             real_missing_value,       __FILE__, __LINE__)
        call assert_equal(record%sfc_wind_qc(1),                  int_missing_value,        __FILE__, __LINE__)
        call assert_equal(record%sfc_wind_pc(1),                  int_missing_value,        __FILE__, __LINE__)
        exit
      end if
      call record_iterator%next()
    end select
  end do

  call test_case_report('Test SYNOP decoder')

  call test_case_final()

end program decoder_test