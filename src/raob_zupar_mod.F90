! RAOB - RAdiosonde OBservation

module raob_zupar_mod

  use datetime
  use container
  use flogger
  use params_mod
  use utils_mod
  use raob_mod

  implicit none

  type(hash_table_type) stations
  type(linked_list_type) records

end module raob_zupar_mod