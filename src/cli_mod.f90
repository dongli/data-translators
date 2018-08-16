module cli_mod

  implicit none

  private

  public cli_get_file_path

contains

  function cli_get_file_path() result(res)

    character(256) res

    logical file_exist

    call get_command_argument(1, res)

    inquire(file=res, exist=file_exist)
    if (.not. file_exist) then
      write(*, *) '[Error]: File ' // trim(res) // ' does not exist!'
      stop 1
    end if

  end function cli_get_file_path

end module cli_mod