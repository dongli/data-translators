cmake_minimum_required(VERSION 2.8)

project(longrun-decode Fortran)

if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  set (CMAKE_Fortran_FLAGS "-ffree-line-length-none")
endif ()
if (CMAKE_SYSTEM_NAME STREQUAL "Darwin")
  SET(CMAKE_Fortran_ARCHIVE_FINISH "<CMAKE_RANLIB> -no_warning_for_no_symbols -c <TARGET>")
endif ()

add_subdirectory(lib/datetime)

add_subdirectory(lib/container)
include_directories(${CMAKE_BINARY_DIR}/fortran_container)

add_subdirectory (lib/unit-test)
include_directories (${UNIT_TEST_INCLUDE_DIR})

if (DEFINED ENV{BUFRLIB} AND (NOT DEFINED ENV{BUFRLIB_ROOT}))
  set(ENV{BUFRLIB_ROOT} $ENV{BUFRLIB})
endif ()
if (DEFINED ENV{BUFRLIB_ROOT})
  include_directories("$ENV{BUFRLIB_ROOT}/include")
  link_directories("$ENV{BUFRLIB_ROOT}/lib")
else ()
  message(FATAL_ERROR "You should set BUFRLIB_ROOT where BUFRLIB is installed!")
endif ()

if (DEFINED ENV{ECCODES} AND (NOT DEFINED ENV{ECCODES_ROOT}))
  set(ENV{ECCODES_ROOT} $ENV{ECCODES})
endif ()
if (DEFINED ENV{ECCODES_ROOT})
  include_directories("$ENV{ECCODES_ROOT}/include")
  link_directories("$ENV{ECCODES_ROOT}/lib")
else ()
  message(FATAL_ERROR "You should set ECCODES_ROOT where ecCodes is installed!")
endif ()

if (DEFINED ENV{ODB_API} AND (NOT DEFINED ENV{ODB_API_ROOT}))
  set(ENV{ODB_API_ROOT} $ENV{ODB_API})
endif ()
if (DEFINED ENV{ODB_API_ROOT})
  include_directories("$ENV{ODB_API_ROOT}/include")
  link_directories("$ENV{ODB_API_ROOT}/lib")
else ()
  message(FATAL_ERROR "You should set ODB_API_ROOT where ODB-API is installed!")
endif ()

set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/.mods")

add_library(data-translators
  src/obs_base_mod.F90
  src/amdar_mod.F90
  src/amdar_bufr_mod.F90
  src/amdar_prepbufr_mod.F90
  src/amdar_odb_mod.F90
  src/amdar_littler_mod.F90
  src/raob_mod.F90
  src/raob_prepbufr_mod.F90
  src/raob_littler_mod.F90
  src/profiler_mod.F90
  src/profiler_prepbufr_mod.F90
  src/profiler_littler_mod.F90
  src/synop_mod.F90
  src/synop_prepbufr_mod.F90
  src/synop_odb_mod.F90
  src/synop_littler_mod.F90
  src/metar_mod.F90
  src/metar_prepbufr_mod.F90
  src/metar_odb_mod.F90
  src/radar_mod.F90
  src/string_mod.F90
  src/cli_mod.F90
  src/params_mod.F90
  src/utils_mod.F90
)
target_link_libraries(data-translators fortran_datetime fortran_container eccodes_f90 bufr Odb_fortran)

add_executable(data-translate.exe src/data_translate.F90)
target_link_libraries(data-translate.exe data-translators)

add_executable(reader-test.exe src/reader_test.F90)
target_link_libraries(reader-test.exe data-translators fortran_unit_test)

add_executable(utils-test.exe src/utils_test.F90)
target_link_libraries(utils-test.exe data-translators fortran_unit_test)
