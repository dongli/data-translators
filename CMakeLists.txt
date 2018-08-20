cmake_minimum_required(VERSION 2.8)

project(longrun-decode Fortran)

if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  set (CMAKE_Fortran_FLAGS "-fno-underscoring -ffree-line-length-none")
endif ()
SET(CMAKE_Fortran_ARCHIVE_FINISH "<CMAKE_RANLIB> -no_warning_for_no_symbols -c <TARGET>")

add_subdirectory(lib/datetime)

add_subdirectory(lib/container)
include_directories(${CMAKE_BINARY_DIR}/fortran_container)

include_directories("/opt/software/gnu/include")
link_directories("/opt/software/gnu/lib")

set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/.mods")

add_library(decoders
  src/obs_base_mod.f90
  src/amdar_mod.f90
  src/amdar_bufr_mod.f90
  src/synop_mod.f90
  src/synop_prepbufr_mod.f90
  src/radar_mod.f90
  src/string_mod.f90
  src/cli_mod.f90
  src/params_mod.f90
  src/utils_mod.f90
)
target_link_libraries(decoders fortran_datetime fortran_container eccodes_f90 bufr Odb_fortran)

add_executable(longrun-decode.exe src/longrun_decode.f90)
target_link_libraries(longrun-decode.exe decoders)
