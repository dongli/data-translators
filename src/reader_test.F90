program reader_test

  use unit_test
  use linked_list_mod
  use synop_mod
#ifdef HAS_LIB_BUFRLIB
  use synop_prepbufr_mod
  use metar_prepbufr_mod
  use amdar_prepbufr_mod
  use raob_prepbufr_mod
#endif
#ifdef HAS_LIB_ECCODES
  use amdar_bufr_mod
#endif

  implicit none

  type(hash_table_type) sites
  type(linked_list_type) records
  type(linked_list_iterator_type) record_iterator

  call test_suite_init('Test readers')

  call test_case_create('SYNOP reader')

#ifdef HAS_LIB_BUFRLIB
  call synop_prepbufr_read('./gdas.20190515/gdas.t00z.prepbufr.nr', sites, records)

  record_iterator = linked_list_iterator(records)
  do while (.not. record_iterator%ended())
    select type (record => record_iterator%value)
    type is (synop_record_type)
      if (record%station%name == '54511') then
        call assert_equal(record%temperature,          19.2,          __FILE__, __LINE__)
        call assert_equal(record%specific_humidity,    9032.0,        __FILE__, __LINE__)
        call assert_equal(record%dewpoint,             12.5,          __FILE__, __LINE__)
        call assert_equal(record%pressure,             100360.00,     __FILE__, __LINE__)
        call assert_equal(record%wind_u,               -2.0,          __FILE__, __LINE__)
        call assert_equal(record%wind_v,               0.0,           __FILE__, __LINE__)
        exit
      end if
      call record_iterator%next()
    end select
  end do
#endif

  call test_suite_report()

  call test_case_create('METAR reader')

#ifdef HAS_LIB_BUFRLIB
  call metar_prepbufr_read('./gdas.20190515/gdas.t00z.prepbufr.nr', sites, records)
#endif

  call test_suite_final()

end program reader_test
