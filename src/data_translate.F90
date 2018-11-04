program data_translate

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

  character(256) input_file_path
  character(256) output_file_path
  character(50) writer_type

  input_file_path = cli_get_input_file_path()
  output_file_path = cli_get_output_file_path()
  writer_type = cli_get_writer_type()

  select case (cli_get_reader_type())
  case ('synop_prepbufr')
    call synop_prepbufr_read(input_file_path)
    if (writer_type == 'odb') then
      call synop_odb_write(output_file_path)
    else if (writer_type == 'littler') then
      call synop_littler_write(output_file_path)
    end if
  case ('metar_prepbufr')
    call metar_prepbufr_read(input_file_path)
    if (writer_type == 'odb') then
      call metar_odb_write(output_file_path)
    else if (writer_type == 'littler') then
      call metar_littler_write(output_file_path)
    end if
  case ('amdar_bufr')
    call amdar_bufr_read(input_file_path)
    if (writer_type == 'odb') then
      call amdar_odb_write(output_file_path)
    else if (writer_type == 'littler') then
      call amdar_littler_write(output_file_path)
    end if
  case ('amdar_prepbufr')
    call amdar_prepbufr_read(input_file_path)
    if (writer_type == 'odb') then
      call amdar_odb_write(output_file_path)
    else if (writer_type == 'littler') then
      call amdar_littler_write(output_file_path)
    end if
  case ('raob_prepbufr')
    call raob_prepbufr_read(input_file_path)
    if (writer_type == 'odb') then
      call raob_odb_write(output_file_path)
    else if (writer_type == 'littler') then
      call raob_littler_write(output_file_path)
    end if
  case ('profiler_prepbufr')
    call profiler_prepbufr_read(input_file_path)
    if (writer_type == 'littler') then
      call profiler_littler_write(output_file_path)
    end if
  case ('ship_cimiss_txt')
    call ship_cimiss_txt_read(input_file_path)
    if (writer_type == 'odb') then
      call ship_odb_write(output_file_path)
    else if (writer_type == 'littler') then
      call ship_littler_write(output_file_path)
    end if
  case ('ship_prepbufr')
    call ship_prepbufr_read(input_file_path)
    if (writer_type == 'odb') then
      call ship_odb_write(output_file_path)
    else if (writer_type == 'littler') then
      call ship_littler_write(output_file_path)
    end if
  case default
    write(*, *) '[Error]: Unknown reader type!'
    stop 1
  end select

  if (output_file_path /= '') then
    write(*, *) '[Notice]: Data ' // trim(output_file_path) // ' is created.'
  end if

end program data_translate
