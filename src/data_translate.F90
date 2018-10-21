program data_translate

  use synop_prepbufr_mod
  use synop_odb_mod
  use synop_littler_mod
  use metar_prepbufr_mod
  use metar_odb_mod
  use amdar_bufr_mod
  use amdar_prepbufr_mod
  use amdar_odb_mod
  use raob_prepbufr_mod
  use profiler_prepbufr_mod
  use cli_mod

  implicit none

  character(256) file_path
  character(50) writer_type

  file_path = cli_get_file_path()
  writer_type = cli_get_writer_type()

  select case (cli_get_reader_type())
  case ('synop_prepbufr')
    call synop_prepbufr_read(file_path)
    if (writer_type == 'synop_odb') then
      call synop_odb_write()
    else if (writer_type == 'synop_littler') then
      call synop_littler_write()
    end if
  case ('metar_prepbufr')
    call metar_prepbufr_read(file_path)
    call metar_odb_write()
  case ('amdar_bufr')
    call amdar_bufr_read(file_path)
    call amdar_odb_write()
  case ('amdar_prepbufr')
    call amdar_prepbufr_read(file_path)
    call amdar_odb_write()
  case ('raob_prepbufr')
    call raob_prepbufr_read(file_path)
  case ('profiler_prepbufr')
    call profiler_prepbufr_read(file_path)
  case default
    write(*, *) '[Error]: Unknown reader type!'
    stop 1
  end select

end program data_translate
