program longrun_decode

  use synop_prepbufr_mod
  use synop_odb_mod
  use amdar_bufr_mod
  use amdar_prepbufr_mod
  use amdar_odb_mod
  use cli_mod

  implicit none

  character(256) file_path

  file_path = cli_get_file_path()

  select case (cli_get_decoder_type())
  case ('synop_prepbufr')
    call synop_prepbufr_decode(file_path)
    call synop_odb_write()
  case ('amdar_bufr')
    call amdar_bufr_decode(file_path)
    call amdar_odb_write()
  case ('amdar_prepbufr')
    call amdar_prepbufr_decode(file_path)
    call amdar_odb_write()
  case default
    write(*, *) '[Error]: Unknown decoder type!'
    stop 1
  end select

end program longrun_decode