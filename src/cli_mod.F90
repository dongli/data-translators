module cli_mod

  implicit none

  private

  public cli_get_reader_type
  public cli_get_writer_type
  public cli_get_input_file_path
  public cli_get_output_file_path
  public cli_get_first_file_path
  public cli_get_second_file_path

contains

  function cli_get_reader_type() result(res)

    character(30) res

    character(256) arg
    integer i

    i = 1
    do while (i <= command_argument_count())
      call get_command_argument(i, arg)
      i = i + 1
      if (arg == '-r' .or. arg == '--reader') then
        call get_command_argument(i, res)
        return
      end if
    end do

  end function cli_get_reader_type

  function cli_get_writer_type() result(res)

    character(30) res

    character(256) arg
    integer i

    i = 1
    do while (i <= command_argument_count())
      call get_command_argument(i, arg)
      i = i + 1
      if (arg == '-w' .or. arg == '--writer') then
        call get_command_argument(i, res)
        return
      end if
    end do

  end function cli_get_writer_type

  function cli_get_input_file_path() result(res)

    character(256) res

    integer i
    logical file_exist

    i = 1
    do while (i <= command_argument_count())
      call get_command_argument(i, res)
      i = i + 1
      if (res == '-i') then
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
    write(*, *) '[Error]: No input data file is provided!'
    stop 1

  end function cli_get_input_file_path

  function cli_get_first_file_path() result(res)

    character(256) res

    integer i
    logical file_exist

    i = 1
    do while (i <= command_argument_count())
      call get_command_argument(i, res)
      i = i + 1
      if (res == '-f1') then
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
    write(*, *) '[Error]: No input data file is provided!'
    stop 1

  end function cli_get_first_file_path

  function cli_get_second_file_path() result(res)

    character(256) res

    integer i
    logical file_exist

    i = 1
    do while (i <= command_argument_count())
      call get_command_argument(i, res)
      i = i + 1
      if (res == '-f2') then
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
    write(*, *) '[Error]: No input data file is provided!'
    stop 1

  end function cli_get_second_file_path

  function cli_get_output_file_path() result(res)

    character(256) res

    integer i

    i = 1
    do while (i <= command_argument_count())
      call get_command_argument(i, res)
      i = i + 1
      if (res == '-o') then
        call get_command_argument(i, res)
        return
      end if
    end do
    res = ''

  end function cli_get_output_file_path

end module cli_mod
