program data_translate

  use hash_table_mod
  use linked_list_mod
  use synop_prepbufr_mod
  use synop_odb_mod
  use synop_littler_mod
  use metar_prepbufr_mod
  use metar_odb_mod
  use metar_littler_mod
  use amdar_bufr_mod
  use amdar_prepbufr_mod
  use amdar_odb_mod
  use amdar_littler_mod
  use amdar_compare_mod
  use raob_prepbufr_mod
  use raob_odb_mod
  use raob_littler_mod
  use profiler_prepbufr_mod
  use profiler_littler_mod
  use ship_cimiss_txt_mod
  use ship_prepbufr_mod
  use ship_odb_mod
  use ship_littler_mod
  use cli_mod

  implicit none

  character(256) first_file_path
  character(256) second_file_path
  character(256) output_file_path

  type(hash_table_type) sites1, sites2
  type(linked_list_type) records1, records2

  first_file_path = cli_get_first_file_path()
  second_file_path = cli_get_second_file_path()
  output_file_path = cli_get_output_file_path()

  select case (cli_get_reader_type())
  case ('synop_prepbufr')
    call synop_prepbufr_read(first_file_path, sites1, records1)
  case ('metar_prepbufr')
    call metar_prepbufr_read(first_file_path, sites1, records1)
  case ('amdar_bufr')
    call amdar_bufr_read(first_file_path, sites1, records1)
  case ('amdar_prepbufr')
    call amdar_prepbufr_read(first_file_path, sites1, records1)
    call amdar_prepbufr_read(second_file_path, sites2, records2)
    call amdar_compare_run(sites1, records1, sites2, records2, output_file_path)
  case ('raob_prepbufr')
    call raob_prepbufr_read(first_file_path, sites1, records1)
  case ('profiler_prepbufr')
    call profiler_prepbufr_read(first_file_path, sites1, records1)
  case ('ship_cimiss_txt')
    call ship_cimiss_txt_read(first_file_path, sites1, records1)
  case ('ship_prepbufr')
    call ship_prepbufr_read(first_file_path, sites1, records1)
  case default
    write(*, *) '[Error]: Unknown reader type!'
    stop 1
  end select

end program data_translate
