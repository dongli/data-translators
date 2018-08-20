program longrun_decode

  use synop_prepbufr_mod
  use amdar_bufr_mod
  use cli_mod

  implicit none

  character(256) file_path

  file_path = cli_get_file_path()

  select case (cli_get_decoder_type())
  case ('synop_prepbufr')
    call synop_prepbufr_decode(file_path)
  case ('amdar_bufr')
    call amdar_bufr_decode(file_path)
  case default
    write(*, *) '[Error]: Unknown decoder type!'
    stop 1
  end select

end program longrun_decode