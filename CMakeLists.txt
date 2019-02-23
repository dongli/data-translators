cmake_minimum_required(VERSION 2.8)

project(data-translators Fortran)

if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  set (CMAKE_Fortran_FLAGS "-ffree-line-length-none -g -fbacktrace")
endif ()
if (CMAKE_SYSTEM_NAME STREQUAL "Darwin")
  SET(CMAKE_Fortran_ARCHIVE_FINISH "<CMAKE_RANLIB> -no_warning_for_no_symbols -c <TARGET>")
endif ()

if (${CMAKE_PROJECT_NAME} STREQUAL ${PROJECT_NAME})
  add_subdirectory(lib/datetime)
endif ()

if (${CMAKE_PROJECT_NAME} STREQUAL ${PROJECT_NAME})
  add_subdirectory(lib/container)
  include_directories(${CMAKE_BINARY_DIR}/fortran_container)
endif ()

if (${CMAKE_PROJECT_NAME} STREQUAL ${PROJECT_NAME})
  add_subdirectory(lib/unit-test)
  include_directories(${UNIT_TEST_INCLUDE_DIR})
endif ()

add_subdirectory(lib/regex)

if (DEFINED ENV{BUFRLIB} AND (NOT DEFINED ENV{BUFRLIB_ROOT}))
  set(ENV{BUFRLIB_ROOT} $ENV{BUFRLIB})
endif ()
if (DEFINED ENV{BUFRLIB_ROOT})
  include_directories("$ENV{BUFRLIB_ROOT}/include")
  link_directories("$ENV{BUFRLIB_ROOT}/lib")
  list(APPEND DATA_TRANSLATORS_INC $ENV{BUFRLIB_ROOT}/include)
  list(APPEND DATA_TRANSLATORS_LIB $ENV{BUFRLIB_ROOT}/lib)
else ()
  message(FATAL_ERROR "You should set BUFRLIB_ROOT where BUFRLIB is installed!")
endif ()

if (DEFINED ENV{ECCODES} AND (NOT DEFINED ENV{ECCODES_ROOT}))
  set(ENV{ECCODES_ROOT} $ENV{ECCODES})
endif ()
if (DEFINED ENV{ECCODES_ROOT})
  include_directories("$ENV{ECCODES_ROOT}/include")
  link_directories("$ENV{ECCODES_ROOT}/lib")
  list(APPEND DATA_TRANSLATORS_INC $ENV{ECCODES_ROOT}/include)
  list(APPEND DATA_TRANSLATORS_LIB $ENV{ECCODES_ROOT}/lib)
else ()
  message(FATAL_ERROR "You should set ECCODES_ROOT where ecCodes is installed!")
endif ()

if (DEFINED ENV{ODB_API} AND (NOT DEFINED ENV{ODB_API_ROOT}))
  set(ENV{ODB_API_ROOT} $ENV{ODB_API})
endif ()
if (DEFINED ENV{ODB_API_ROOT})
  include_directories("$ENV{ODB_API_ROOT}/include")
  link_directories("$ENV{ODB_API_ROOT}/lib")
  list(APPEND DATA_TRANSLATORS_INC $ENV{ODB_API_ROOT}/include)
  list(APPEND DATA_TRANSLATORS_LIB $ENV{ODB_API_ROOT}/lib)
else ()
  message(FATAL_ERROR "You should set ODB_API_ROOT where ODB-API is installed!")
endif ()

if (DEFINED ENV{NETCDF} AND (NOT DEFINED ENV{NETCDF_ROOT}))
  set(ENV{NETCDF_ROOT} $ENV{NETCDF})
endif ()
if (DEFINED ENV{NETCDF_ROOT})
  include_directories("$ENV{NETCDF_ROOT}/include")
  link_directories("$ENV{NETCDF_ROOT}/lib")
  list(APPEND DATA_TRANSLATORS_INC $ENV{NETCDF_ROOT}/include)
  list(APPEND DATA_TRANSLATORS_LIB $ENV{NETCDF_ROOT}/lib)
else ()
  message(FATAL_ERROR "You should set NETCDF_ROOT where NetCDF is installed!")
endif ()

if (DEFINED ENV{FOX} AND (NOT DEFINED ENV{FOX_ROOT}))
  set(ENV{FOX_ROOT} $ENV{FOX})
endif ()
if (DEFINED ENV{FOX_ROOT})
  include_directories("$ENV{FOX_ROOT}/include")
  link_directories("$ENV{FOX_ROOT}/lib")
  list(APPEND DATA_TRANSLATORS_INC $ENV{FOX_ROOT}/include)
  list(APPEND DATA_TRANSLATORS_LIB $ENV{FOX_ROOT}/lib)
else ()
  message(FATAL_ERROR "You should set FOX_ROOT where FOX is installed!")
endif ()

if (NOT ${CMAKE_PROJECT_NAME} STREQUAL ${PROJECT_NAME})
  set(DATA_TRANSLATORS_INC ${DATA_TRANSLATORS_INC} PARENT_SCOPE)
  set(DATA_TRANSLATORS_LIB ${DATA_TRANSLATORS_LIB} PARENT_SCOPE)
endif ()

set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/.mods")

add_library(data_translators
  src/obs_base_mod.F90
  src/amdar_mod.F90
  src/amdar_bufr_mod.F90
  src/amdar_prepbufr_mod.F90
  src/amdar_cimiss_xml_mod.F90
  src/amdar_odb_mod.F90
  src/amdar_littler_mod.F90
  src/raob_mod.F90
  src/raob_prepbufr_mod.F90
  src/raob_littler_mod.F90
  src/raob_odb_mod.F90
  src/raob_cimiss_xml_mod.F90
  src/profiler_mod.F90
  src/profiler_prepbufr_mod.F90
  src/profiler_odb_mod.F90
  src/profiler_littler_mod.F90
  src/synop_mod.F90
  src/synop_prepbufr_mod.F90
  src/synop_odb_mod.F90
  src/synop_littler_mod.F90
  src/synop_cimiss_xml_mod.F90
  src/synop_netcdf_mod.F90
  src/metar_mod.F90
  src/metar_prepbufr_mod.F90
  src/metar_odb_mod.F90
  src/metar_littler_mod.F90
  src/ship_mod.F90
  src/ship_cimiss_txt_mod.F90
  src/ship_prepbufr_mod.F90
  src/ship_odb_mod.F90
  src/ship_littler_mod.F90
  src/radar_mod.F90
  src/string_mod.F90
  src/cli_mod.F90
  src/params_mod.F90
  src/missing_value_mod.F90
  src/utils_mod.F90
)
target_link_libraries(data_translators
  fortran_datetime
  fortran_container
  fortran_regex
  eccodes_f90
  bufr
  Odb_fortran
  netcdff
  FoX_sax
  FoX_common
  FoX_fsys
  FoX_utils
)

add_executable(data_translate.exe src/data_translate.F90)
target_link_libraries(data_translate.exe data_translators)

if (EXISTS lib/unit-test/CMakeLists)
  add_executable(reader_test.exe src/reader_test.F90)
  target_link_libraries(reader_test.exe data_translators fortran_unit_test)

  add_executable(utils_test.exe src/utils_test.F90)
  target_link_libraries(utils_test.exe data_translators fortran_unit_test)
endif ()
