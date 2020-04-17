program data_translate

  use container
  use string
  use flogger
#ifdef HAS_LIB_BUFRLIB
  use synop_prepbufr_mod
  use metar_prepbufr_mod
  use amdar_prepbufr_mod
  use raob_prepbufr_mod
  use profiler_prepbufr_mod
  use ship_prepbufr_mod
#endif
#ifdef HAS_LIB_ECCODES
  use amdar_bufr_mod
#endif
#ifdef HAS_LIB_ODB_API
  use synop_odb_mod
  use metar_odb_mod
  use amdar_odb_mod
  use raob_odb_mod
  use profiler_odb_mod
  use ship_odb_mod
#endif
#ifdef HAS_LIB_FOX
  use synop_cimiss_xml_mod
  use amdar_cimiss_xml_mod
  use raob_cimiss_xml_mod
  use ship_cimiss_xml_mod
#endif
#ifdef HAS_LIB_NETCDF
  use synop_netcdf_mod
  use ship_netcdf_mod
  use anem_nrg_netcdf_mod
#endif
#ifdef HAS_LIB_MONGO
  use anem_nrg_mongo_mod
#endif
  use synop_txt_mod
  use synop_littler_mod
  use synop_ftm_txt_mod
  use synop_a_txt_mod
  use metar_littler_mod
  use amdar_littler_mod
  use raob_littler_mod
  use profiler_zrada_mod
  use profiler_littler_mod
  use ship_cimiss_txt_mod
  use ship_txt_mod
  use ship_littler_mod
  use anem_nrg_txt_mod
  use anem_nrg_littler_mod
  use cli_mod

  implicit none

  type(hash_table_type) platforms
  type(linked_list_type) records

  integer iostat

  call print_help()
  call cli_parse_args()

  platforms = hash_table(chunk_size=50000, max_load_factor=0.9)

  if (cli_input_list_file_path /= '') then
    open(20, file=cli_input_list_file_path, status='old')
    do while (.true.)
      read(20, '(A)', iostat=iostat) cli_input_file_path
      if (iostat /= 0) exit
      call input_one_file()
    end do
    close(20)
  else
    call input_one_file()
  end if

  call output_one_file()

contains

  subroutine input_one_file()

    select case (cli_reader_type)
#ifdef HAS_LIB_BUFRLIB
    case ('synop_prepbufr')
      call synop_prepbufr_read(cli_input_file_path, platforms, records)
    case ('metar_prepbufr')
      call metar_prepbufr_read(cli_input_file_path, platforms, records)
    case ('amdar_prepbufr')
      call amdar_prepbufr_read(cli_input_file_path, platforms, records)
    case ('raob_prepbufr')
      call raob_prepbufr_read(cli_input_file_path, platforms, records)
    case ('profiler_prepbufr')
      call profiler_prepbufr_read(cli_input_file_path, platforms, records)
    case ('ship_prepbufr')
      call ship_prepbufr_read(cli_input_file_path, platforms, records)
#endif
#ifdef HAS_LIB_ECCODES
    case ('amdar_bufr')
      call amdar_bufr_read(cli_input_file_path, platforms, records)
#endif
#ifdef HAS_LIB_FOX
    case ('synop_cimiss_xml')
      call synop_cimiss_xml_read(cli_input_file_path, platforms, records)
    case ('amdar_cimiss_xml')
      call amdar_cimiss_xml_read(cli_input_file_path, platforms, records)
    case ('raob_cimiss_xml')
      call raob_cimiss_xml_read(cli_input_file_path, platforms, records)
    case ('ship_cimiss_xml')
      call ship_cimiss_xml_read(cli_input_file_path, platforms, records)
    case ('ship_cimiss_txt')
      call ship_cimiss_txt_read(cli_input_file_path, platforms, records)
#endif
    case ('synop_ftm_txt')
      call synop_ftm_txt_read(cli_input_file_path, platforms, records)
    case ('synop_a_txt')
      call synop_a_txt_read(cli_input_file_path, platforms, records)
    case ('profiler_zrada')
      call profiler_zrada_read(cli_input_file_path, platforms, records)
    case ('ship_txt')
      call ship_txt_read(cli_input_file_path, platforms, records)
    case ('synop_txt')
      call synop_txt_read(cli_input_file_path, platforms, records)
    case ('anem_nrg_txt')
      call anem_nrg_txt_read(cli_input_file_path, platforms, records)
    case default
      call log_error('Unknown reader type!')
    end select
  
  end subroutine input_one_file

  subroutine output_one_file()

    if (index(cli_reader_type, 'synop') == 1) then
      select case (cli_writer_type)
      case ('littler')
        call synop_littler_write(cli_output_file_path, platforms, records)
#ifdef HAS_LIB_ODB_API
      case ('odb')
        call synop_odb_write(cli_output_file_path, platforms, records)
#endif
#ifdef HAS_LIB_NETCDF
      case ('netcdf')
        call synop_netcdf_write(cli_output_file_path, platforms, records)
#endif
#ifdef HAS_LIB_BUFRLIB
      case ('prepbufr')
        call synop_prepbufr_write(cli_output_file_path, platforms, records)
#endif
      end select
    else if (index(cli_reader_type, 'metar') == 1) then
      select case (cli_writer_type)
      case ('littler')
        call metar_littler_write(cli_output_file_path, platforms, records)
#ifdef HAS_LIB_ODB_API
      case ('odb')
        call metar_odb_write(cli_output_file_path, platforms, records)
#endif
      end select
    else if (index(cli_reader_type, 'raob') == 1) then
      select case (cli_writer_type)
      case ('littler')
        call raob_littler_write(cli_output_file_path, platforms, records)
#ifdef HAS_LIB_ODB_API
      case ('odb')
        call raob_odb_write(cli_output_file_path, platforms, records)
#endif
      end select
    else if (index(cli_reader_type, 'amdar') == 1) then
      select case (cli_writer_type)
      case ('littler')
        call amdar_littler_write(cli_output_file_path, platforms, records)
#ifdef HAS_LIB_ODB_API
      case ('odb')
        call amdar_odb_write(cli_output_file_path, platforms, records)
#endif
      end select
    else if (index(cli_reader_type, 'profiler') == 1) then
      select case (cli_writer_type)
      case ('littler')
        call profiler_littler_write(cli_output_file_path, platforms, records)
#ifdef HAS_LIB_ODB_API
      case ('odb')
        call profiler_odb_write(cli_output_file_path, platforms, records)
#endif
      end select
    else if (index(cli_reader_type, 'ship') == 1) then
      select case (cli_writer_type)
      case ('littler')
        call ship_littler_write(cli_output_file_path, platforms, records)
#ifdef HAS_LIB_ODB_API
      case ('odb')
        call ship_odb_write(cli_output_file_path, platforms, records)
#endif
#ifdef HAS_LIB_NETCDF
      case ('netcdf')
        call ship_netcdf_write(cli_output_file_path, platforms, records)
#endif
      end select
    else if (index(cli_reader_type, 'anem_nrg') == 1) then
      select case (cli_writer_type)
      case ('littler')
        call anem_nrg_littler_write(cli_output_file_path, platforms, records)
#ifdef HAS_LIB_MONGO
      case ('mongo')
        call anem_nrg_mongo_write(cli_output_file_path, platforms, records)
#endif
#ifdef HAS_LIB_NETCDF
      case ('netcdf')
        call anem_nrg_netcdf_write(cli_output_file_path, platforms, records)
#endif
      end select
    end if

    if (cli_output_file_path /= '') then
      call log_notice('Data ' // trim(cli_output_file_path) // ' is created.')
    end if

  end subroutine output_one_file

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
