! RAOB - RAdiosonde OBservation

module raob_zupar_mod

  use raob_mod
  use datetime_mod
  use hash_table_mod
  use linked_list_mod
  use params_mod
  use utils_mod
  use odbql_wrappers

  implicit none

  type(hash_table_type) stations
  type(linked_list_type) records

end module raob_zupar_mod