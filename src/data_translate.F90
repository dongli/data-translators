program data_translate

  use linked_list_mod
  use hash_table_mod
  use synop_prepbufr_mod
  use synop_odb_mod
  use synop_littler_mod
  use synop_cimiss_xml_mod
  use synop_netcdf_mod
  use metar_prepbufr_mod
  use metar_odb_mod
  use metar_littler_mod
  use amdar_bufr_mod
  use amdar_prepbufr_mod
  use amdar_odb_mod
  use amdar_cimiss_xml_mod
  use amdar_littler_mod
  use raob_prepbufr_mod
  use raob_odb_mod
  use raob_littler_mod
  use raob_cimiss_xml_mod
  use profiler_prepbufr_mod
  use profiler_zrada_mod
  use profiler_odb_mod
  use profiler_littler_mod
  use ship_cimiss_txt_mod
  use ship_cimiss_xml_mod
  use ship_txt_mod
  use ship_prepbufr_mod
  use ship_odb_mod
  use ship_littler_mod
  use ship_netcdf_mod
  use cli_mod

  implicit none

  type(hash_table_type) sites
  type(linked_list_type) records

  call print_help()
  call cli_parse_args()

  select case (cli_reader_type)
  case ('synop_prepbufr')
    call synop_prepbufr_read(cli_input_file_path, sites, records)
    if (cli_writer_type == 'odb') then
      call synop_odb_write(cli_output_file_path, sites, records)
    else if (cli_writer_type == 'littler') then
      call synop_littler_write(cli_output_file_path, sites, records)
    else if (cli_writer_type == 'netcdf') then
      call synop_netcdf_write(cli_output_file_path, sites, records)
    end if
  case ('synop_cimiss_xml')
    call synop_cimiss_xml_read(cli_input_file_path, sites, records)
    if (cli_writer_type == 'odb') then
      call synop_odb_write(cli_output_file_path, sites, records)
    else if (cli_writer_type == 'littler') then
      call synop_littler_write(cli_output_file_path, sites, records)
    else if (cli_writer_type == 'prepbufr') then
      call synop_prepbufr_write(cli_output_file_path, sites, records)
    end if
  case ('metar_prepbufr')
    call metar_prepbufr_read(cli_input_file_path, sites, records)
    if (cli_writer_type == 'odb') then
      call metar_odb_write(cli_output_file_path, sites, records)
    else if (cli_writer_type == 'littler') then
      call metar_littler_write(cli_output_file_path, sites, records)
    end if
  case ('amdar_bufr')
    call amdar_bufr_read(cli_input_file_path, sites, records)
    if (cli_writer_type == 'odb') then
      call amdar_odb_write(cli_output_file_path, sites, records)
    else if (cli_writer_type == 'littler') then
      call amdar_littler_write(cli_output_file_path, sites, records)
    end if
  case ('amdar_prepbufr')
    call amdar_prepbufr_read(cli_input_file_path, sites, records)
    if (cli_writer_type == 'odb') then
      call amdar_odb_write(cli_output_file_path, sites, records)
    else if (cli_writer_type == 'littler') then
      call amdar_littler_write(cli_output_file_path, sites, records)
    end if
  case ('amdar_cimiss_xml')
    call amdar_cimiss_xml_read(cli_input_file_path, sites, records)
    if (cli_writer_type == 'odb') then
      call amdar_odb_write(cli_output_file_path, sites, records)
    else if (cli_writer_type == 'littler') then
      call amdar_littler_write(cli_output_file_path, sites, records)
    end if
  case ('raob_prepbufr')
    call raob_prepbufr_read(cli_input_file_path, sites, records)
    if (cli_writer_type == 'odb') then
      call raob_odb_write(cli_output_file_path, sites, records)
    else if (cli_writer_type == 'littler') then
      call raob_littler_write(cli_output_file_path, sites, records)
    end if
  case ('raob_cimiss_xml')
    call raob_cimiss_xml_read(cli_input_file_path, sites, records)
    if (cli_writer_type == 'odb') then
      call raob_odb_write(cli_output_file_path, sites, records)
    else if (cli_writer_type == 'littler') then
      call raob_littler_write(cli_output_file_path, sites, records)
    end if
  case ('profiler_prepbufr')
    call profiler_prepbufr_read(cli_input_file_path, sites, records)
    if (cli_writer_type == 'odb') then
      call profiler_odb_write(cli_output_file_path, sites, records)
    else if (cli_writer_type == 'littler') then
      call profiler_littler_write(cli_output_file_path, sites, records)
    end if
  case ('profiler_zrada')
    call profiler_zrada_read(cli_input_file_path, sites, records)
    if (cli_writer_type == 'odb') then
      call profiler_odb_write(cli_output_file_path, sites, records)
    else if (cli_writer_type == 'littler') then
      call profiler_littler_write(cli_output_file_path, sites, records)
    end if
  case ('ship_cimiss_xml')
    call ship_cimiss_xml_read(cli_input_file_path, sites, records)
    if (cli_writer_type == 'odb') then
      call ship_odb_write(cli_output_file_path, sites, records)
    else if (cli_writer_type == 'littler') then
      call ship_littler_write(cli_output_file_path, sites, records)
    else if (cli_writer_type == 'netcdf') then
      call ship_netcdf_write(cli_output_file_path, sites, records)
    end if
  case ('ship_cimiss_txt')
    call ship_cimiss_txt_read(cli_input_file_path, sites, records)
    if (cli_writer_type == 'odb') then
      call ship_odb_write(cli_output_file_path, sites, records)
    else if (cli_writer_type == 'littler') then
      call ship_littler_write(cli_output_file_path, sites, records)
    end if
  case ('ship_txt')
    call ship_txt_read(cli_input_file_path, sites, records)
    if (cli_writer_type == 'odb') then
      call ship_odb_write(cli_output_file_path, sites, records)
    else if (cli_writer_type == 'littler') then
      call ship_littler_write(cli_output_file_path, sites, records)
    else if (cli_writer_type == 'netcdf') then
      call ship_netcdf_write(cli_output_file_path, sites, records)
    end if
  case ('ship_prepbufr')
    call ship_prepbufr_read(cli_input_file_path, sites, records)
    if (cli_writer_type == 'odb') then
      call ship_odb_write(cli_output_file_path, sites, records)
    else if (cli_writer_type == 'littler') then
      call ship_littler_write(cli_output_file_path, sites, records)
    end if
  case default
    write(*, *) '[Error]: Unknown reader type!'
    stop 1
  end select

  if (cli_output_file_path /= '') then
    write(*, *) '[Notice]: Data ' // trim(cli_output_file_path) // ' is created.'
  end if

contains

  subroutine print_help()

    character(256) arg
    integer i

    i = 1
    do while (i <= command_argument_count())
      call get_command_argument(i, arg)
      i = i + 1
      if (arg == '-h' .or. arg == '--help') then
        write(*, *)
        write(*, *) ' ./data_translate.exe -r <obs_type>_<input_format> -w <output_format> -i <input_obs_file> -o <output_obs_file>'
        write(*, *)
        stop 0
      end if
    end do

  end subroutine print_help


end program data_translate
