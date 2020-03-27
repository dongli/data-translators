module cli_mod

  use datetime
  use flogger

  implicit none

  private

  public cli_parse_args
  public cli_reader_type
  public cli_writer_type
  public cli_input_file_path
  public cli_input_list_file_path
  public cli_output_file_path
  public cli_first_file_path
  public cli_second_file_path
  public cli_verbose_platform
  public cli_start_time
  public cli_end_time

  character(30) :: cli_reader_type = ''
  character(30) :: cli_writer_type = ''
  character(4096) :: cli_input_file_path = ''
  character(4096) :: cli_input_list_file_path = ''
  character(256) :: cli_output_file_path = ''
  character(256) :: cli_first_file_path = ''
  character(256) :: cli_second_file_path = ''
  character(30) :: cli_verbose_platform = 'N/A'
  type(datetime_type) cli_start_time
  type(datetime_type) cli_end_time

contains

  subroutine cli_parse_args()

    character(256) res, str
    logical file_exist
    integer i

    i = 1
    do while (i <= command_argument_count())
      call get_command_argument(i, res)
      select case (res)
      case ('-r', '--reader')
        i = i + 1
        call get_command_argument(i, cli_reader_type)
      case ('-w', '--writer')
        i = i + 1
        call get_command_argument(i, cli_writer_type)
      case ('-i', '--input')
        i = i + 1
        call get_command_argument(i, cli_input_file_path)
      case ('-l', '--input-list')
        i = i + 1
        call get_command_argument(i, cli_input_list_file_path)
      case ('-o', '--output')
        i = i + 1
        call get_command_argument(i, cli_output_file_path)
      case ('-f1')
        i = i + 1
        call get_command_argument(i, cli_first_file_path)
      case ('-f2')
        i = i + 1
        call get_command_argument(i, cli_second_file_path)
      case ('-t1')
        i = i + 1
        call get_command_argument(i, str)
        cli_start_time = create_datetime(str, '%Y%m%d%H%M')
      case ('-t2')
        i = i + 1
        call get_command_argument(i, str)
        cli_end_time = create_datetime(str, '%Y%m%d%H%M')
      case ('-v')
        i = i + 1
        call get_command_argument(i, cli_verbose_platform)
      end select
      i = i + 1
    end do

    if (cli_input_list_file_path /= '') then
      inquire(file=cli_input_list_file_path, exist=file_exist)
      if (.not. file_exist) then
        call log_error('Input list file ' // trim(cli_input_list_file_path) // ' does not exist!')
      end if
    end if
    if (cli_input_file_path /= '') then
      inquire(file=cli_input_file_path, exist=file_exist)
      if (.not. file_exist) then
        call log_error('Input file ' // trim(cli_input_file_path) // ' does not exist!')
      end if
    end if
    if (cli_first_file_path /= '') then
      inquire(file=cli_first_file_path, exist=file_exist)
      if (.not. file_exist) then
        call log_error('First input file ' // trim(cli_first_file_path) // ' does not exist!')
      end if
    end if
    if (cli_second_file_path /= '') then
      inquire(file=cli_second_file_path, exist=file_exist)
      if (.not. file_exist) then
        call log_error('Second input file ' // trim(cli_second_file_path) // ' does not exist!')
      end if
    end if

  end subroutine cli_parse_args

end module cli_mod
