module cli_mod

  implicit none

  private

  public cli_parse_args
  public cli_reader_type
  public cli_writer_type
  public cli_input_file_path
  public cli_output_file_path
  public cli_first_file_path
  public cli_second_file_path
  public cli_verbose_platform

  character(30) :: cli_reader_type = ''
  character(30) :: cli_writer_type = ''
  character(256) :: cli_input_file_path = ''
  character(256) :: cli_output_file_path = ''
  character(256) :: cli_first_file_path = ''
  character(256) :: cli_second_file_path = ''
  character(30) :: cli_verbose_platform = 'N/A'

contains

  subroutine cli_parse_args()

    character(256) res
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
      case ('-o', '--output')
        i = i + 1
        call get_command_argument(i, cli_output_file_path)
      case ('-f1')
        i = i + 1
        call get_command_argument(i, cli_first_file_path)
      case ('-f2')
        i = i + 1
        call get_command_argument(i, cli_second_file_path)
      case ('-v')
        i = i + 1
        call get_command_argument(i, cli_verbose_platform)
      end select
      i = i + 1
    end do

    if (cli_input_file_path /= '') then
      inquire(file=cli_input_file_path, exist=file_exist)
      if (file_exist) then
        return
      else
        write(*, *) '[Error]: Input file ' // trim(cli_input_file_path) // ' does not exist!'
        stop 1
      end if
    end if
    if (cli_first_file_path /= '') then
      inquire(file=cli_first_file_path, exist=file_exist)
      if (file_exist) then
        return
      else
        write(*, *) '[Error]: First input file ' // trim(cli_first_file_path) // ' does not exist!'
        stop 1
      end if
    end if
    if (cli_second_file_path /= '') then
      inquire(file=cli_second_file_path, exist=file_exist)
      if (file_exist) then
        return
      else
        write(*, *) '[Error]: Second input file ' // trim(cli_second_file_path) // ' does not exist!'
        stop 1
      end if
    end if

  end subroutine cli_parse_args

end module cli_mod
