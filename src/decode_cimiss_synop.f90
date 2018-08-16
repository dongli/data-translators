program decode_synop_cimiss

  use synop_mod
  use hash_table_mod
  use linked_list_mod
  use string_mod

  implicit none

  character(256) data_path

  type(hash_table_type) synop_stations
  type(linked_list_type) synop_records

  call get_command_argument(1, data_path)

  call decode(data_path, synop_stations, synop_records)
  call write_laps_netcdf(synop_stations, synop_records)

contains

  subroutine decode(data_path, synop_stations, synop_records)

    character(*), intent(in) :: data_path
    type(hash_table_type), intent(out) :: synop_stations
    type(linked_list_type), intent(out) :: synop_records

    integer i
    character(1024) line

    open(10, file=data_path, status='old')
    read(10, '(A)') line
    if (line(3:5) /= 'xml') then
      write(*, *) '[Error]: Input data is not a CIMISS XML file!'
      stop 1
    end if
    read(10, '(A)') line
    if (line(2:3) /= 'DS') then
      write(*, *) '[Error]: Unexpected second line in the CIMISS XML file!'
      write(*, *) '=> ', trim(line)
      stop 1
    end if
    print *, trim(line)
    close(10)

  end subroutine decode

  subroutine write_laps_netcdf(synop_stations, synop_records)

    type(hash_table_type), intent(in) :: synop_stations
    type(linked_list_type), intent(in) :: synop_records

  end subroutine write_laps_netcdf

end program decode_synop_cimiss
