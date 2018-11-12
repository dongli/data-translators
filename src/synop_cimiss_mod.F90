module synop_cimiss_mod

  use synop_mod
  use datetime_mod
  use timedelta_mod
  use hash_table_mod
  use linked_list_mod
  use string_mod
  use params_mod
  use utils_mod

  implicit none

  private

  public synop_cimiss_read

contains

  subroutine synop_cimiss_read(file_path, stations, records)

    character(*), intent(in) :: file_path
    type(hash_table_type), intent(inout) :: stations
    type(linked_list_type), intent(inout) :: records

    integer i
    character(1024) line

    open(10, file=file_path, status='old')
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

  end subroutine synop_cimiss_read

  subroutine synop_cimiss_write_laps_netcdf()

  end subroutine synop_cimiss_write_laps_netcdf

end module synop_cimiss_mod
