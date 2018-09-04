module cli_mod

  implicit none

  private

  public cli_get_decoder_type
  public cli_get_file_path

contains

  function cli_get_decoder_type() result(res)

    character(30) res

    character(256) arg
    integer i

    i = 1
    do while (i <= command_argument_count())
      call get_command_argument(i, arg)
      i = i + 1
      if (arg == '-d') then
        call get_command_argument(i, res)
        return
      end if
    end do

  end function cli_get_decoder_type

  function cli_get_file_path() result(res)

    character(256) res

    integer i
    logical file_exist

    i = 1
    do while (i <= command_argument_count())
      call get_command_argument(i, res)
      i = i + 1
      if (res == '-f') then
        call get_command_argument(i, res)
        inquire(file=res, exist=file_exist)
        if (file_exist) then
          return
        else
          write(*, *) '[Error]: Data file ' // trim(res) // ' does not exist!'
          stop 1
        end if
      end if
    end do
    write(*, *) '[Error]: No data file is provided!'
    stop 1

  end function cli_get_file_path

end module cli_mod