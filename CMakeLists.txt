cmake_minimum_required(VERSION 2.8)

project(data-translators Fortran)

if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  set (CMAKE_Fortran_FLAGS "-ffree-line-length-none -g -fbacktrace")
endif ()
if (CMAKE_SYSTEM_NAME STREQUAL "Darwin")
  SET(CMAKE_Fortran_ARCHIVE_FINISH "<CMAKE_RANLIB> -no_warning_for_no_symbols -c <TARGET>")
endif ()
set (CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}")
include_directories("${CMAKE_BINARY_DIR}")

if (EXISTS ${PROJECT_SOURCE_DIR}/lib/datetime/CMakeLists.txt)
  add_subdirectory(lib/datetime)
  set(HAS_LIB_DATETIME ON)
endif ()
if (EXISTS ${PROJECT_SOURCE_DIR}/lib/container/CMakeLists.txt)
  add_subdirectory(lib/container)
endif ()
if (EXISTS ${PROJECT_SOURCE_DIR}/lib/unit-test/CMakeLists.txt)
  add_subdirectory(lib/unit-test)
endif ()
if (EXISTS ${PROJECT_SOURCE_DIR}/lib/string/CMakeLists.txt)
  add_subdirectory(lib/string)
endif ()
add_subdirectory(lib/regex)

if (DEFINED ENV{BUFRLIB} AND (NOT DEFINED ENV{BUFRLIB_ROOT}))
  set(ENV{BUFRLIB_ROOT} $ENV{BUFRLIB})
endif ()
if (DEFINED ENV{BUFRLIB_ROOT})
  include_directories("$ENV{BUFRLIB_ROOT}/include")
  link_directories("$ENV{BUFRLIB_ROOT}/lib")
  set(HAS_LIB_BUFRLIB ON)
  list(APPEND DATA_TRANSLATORS_INC $ENV{BUFRLIB_ROOT}/include)
  list(APPEND DATA_TRANSLATORS_LIB $ENV{BUFRLIB_ROOT}/lib)
  add_definitions(-DHAS_LIB_BUFRLIB)
else ()
  message(WARNING "No BUFRLIB is found! Build without it.")
endif ()

if (DEFINED ENV{ECCODES} AND (NOT DEFINED ENV{ECCODES_ROOT}))
  set(ENV{ECCODES_ROOT} $ENV{ECCODES})
endif ()
if (DEFINED ENV{ECCODES_ROOT})
  include_directories("$ENV{ECCODES_ROOT}/include")
  link_directories("$ENV{ECCODES_ROOT}/lib")
  set(HAS_LIB_ECCODES ON)
  list(APPEND DATA_TRANSLATORS_INC $ENV{ECCODES_ROOT}/include)
  list(APPEND DATA_TRANSLATORS_LIB $ENV{ECCODES_ROOT}/lib)
  add_definitions(-DHAS_LIB_ECCODES)
else ()
  message(WARNING "No ECCODES_ROOT is found! Build without it.")
endif ()

if (DEFINED ENV{ODB_API} AND (NOT DEFINED ENV{ODB_API_ROOT}))
  set(ENV{ODB_API_ROOT} $ENV{ODB_API})
endif ()
if (DEFINED ENV{ODB_API_ROOT})
  include_directories("$ENV{ODB_API_ROOT}/include")
  link_directories("$ENV{ODB_API_ROOT}/lib")
  set(HAS_LIB_ODB_API ON)
  list(APPEND DATA_TRANSLATORS_INC $ENV{ODB_API_ROOT}/include)
  list(APPEND DATA_TRANSLATORS_LIB $ENV{ODB_API_ROOT}/lib)
  add_definitions(-DHAS_LIB_ODB_API)
else ()
  message(WARNING "No ODB_API is found! Build without it.")
endif ()

if (DEFINED ENV{NETCDF} AND (NOT DEFINED ENV{NETCDF_ROOT}))
  set(ENV{NETCDF_ROOT} $ENV{NETCDF})
endif ()
if (DEFINED ENV{NETCDF_ROOT})
  include_directories("$ENV{NETCDF_ROOT}/include")
  link_directories("$ENV{NETCDF_ROOT}/lib")
  set(HAS_LIB_NETCDF ON)
  list(APPEND DATA_TRANSLATORS_INC $ENV{NETCDF_ROOT}/include)
  list(APPEND DATA_TRANSLATORS_LIB $ENV{NETCDF_ROOT}/lib)
  add_definitions(-DHAS_LIB_NETCDF)
else ()
  message(WARNING "No NETCDF is found! Build without it.")
endif ()

if (DEFINED ENV{FOX} AND (NOT DEFINED ENV{FOX_ROOT}))
  set(ENV{FOX_ROOT} $ENV{FOX})
endif ()
if (DEFINED ENV{FOX_ROOT})
  include_directories("$ENV{FOX_ROOT}/include")
  link_directories("$ENV{FOX_ROOT}/lib")
  set(HAS_LIB_FOX ON)
  list(APPEND DATA_TRANSLATORS_INC $ENV{FOX_ROOT}/include)
  list(APPEND DATA_TRANSLATORS_LIB $ENV{FOX_ROOT}/lib)
  add_definitions(-DHAS_LIB_FOX)
else ()
  message(WARNING "No FOX is found! Build without it.")
endif ()

set(sources
  src/obs_base_mod.F90
  src/amdar_mod.F90
  src/amdar_littler_mod.F90
  src/raob_mod.F90
  src/raob_littler_mod.F90
  src/profiler_mod.F90
  src/profiler_littler_mod.F90
  src/profiler_zrada_mod.F90
  src/synop_mod.F90
  src/synop_littler_mod.F90
  src/synop_txt_mod.F90
  src/metar_mod.F90
  src/metar_littler_mod.F90
  src/ship_mod.F90
  src/ship_littler_mod.F90
  src/ship_cimiss_txt_mod.F90
  src/ship_txt_mod.F90
  src/radar_mod.F90
  src/cli_mod.F90
  src/params_mod.F90
  src/missing_value_mod.F90
  src/atm_formula_mod.F90
  src/utils_mod.F90
)

if (HAS_LIB_BUFRLIB)
  list(APPEND sources
    src/amdar_prepbufr_mod.F90
    src/raob_prepbufr_mod.F90
    src/profiler_prepbufr_mod.F90
    src/synop_prepbufr_mod.F90
    src/metar_prepbufr_mod.F90
    src/ship_prepbufr_mod.F90
  )
endif ()
if (HAS_LIB_ECCODES)
  list(APPEND sources
    src/amdar_bufr_mod.F90
  )
endif ()
if (HAS_LIB_FOX)
  list(APPEND sources
    src/amdar_cimiss_xml_mod.F90
    src/raob_cimiss_xml_mod.F90
    src/synop_cimiss_xml_mod.F90
    src/ship_cimiss_xml_mod.F90
  )
endif ()
if (HAS_LIB_NETCDF)
  list(APPEND sources
    src/synop_netcdf_mod.F90
    src/ship_netcdf_mod.F90
  )
endif ()
if (HAS_LIB_ODB_API)
  list(APPEND sources
    src/amdar_odb_mod.F90
    src/raob_odb_mod.F90
    src/profiler_odb_mod.F90
    src/synop_odb_mod.F90
    src/metar_odb_mod.F90
    src/ship_odb_mod.F90
  )
endif ()

add_library(data_translators ${sources})
target_link_libraries(data_translators fortran_datetime fortran_container fortran_regex fortran_string)
if (HAS_LIB_ODB_API)
  target_link_libraries(data_translators Odb_fortran)
endif ()
if (HAS_LIB_ECCODES)
  target_link_libraries(data_translators eccodes_f90)
endif ()
if (HAS_LIB_FOX)
  target_link_libraries(data_translators FoX_sax FoX_common FoX_fsys FoX_utils)
endif ()
if (HAS_LIB_BUFRLIB)
  target_link_libraries(data_translators bufr)
endif ()
if (HAS_LIB_NETCDF)
  target_link_libraries(data_translators netcdff)
endif ()

add_executable(data_translate.exe src/data_translate.F90)
target_link_libraries(data_translate.exe data_translators)

set(DATA_TRANSLATORS_INC ${DATA_TRANSLATORS_INC} PARENT_SCOPE)
set(DATA_TRANSLATORS_LIB ${DATA_TRANSLATORS_LIB} PARENT_SCOPE)

if (EXISTS ${PROJECT_SOURCE_DIR}/lib/unit-test/CMakeLists.txt)
  add_executable(reader_test.exe src/reader_test.F90)
  target_link_libraries(reader_test.exe data_translators fortran_unit_test)

  add_executable(test_atm_formula.exe src/test_atm_formula.F90)
  target_link_libraries(test_atm_formula.exe data_translators fortran_unit_test)
endif ()
