cmake_minimum_required(VERSION 2.8)

project(longrun-decoder Fortran)

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

add_library(utils
  src/obs_base_mod.f90
  src/amdar_mod.f90
  src/synop_mod.f90
  src/radar_mod.f90
  src/string_mod.f90
  src/cli_mod.f90
  src/params_mod.f90
  src/utils_mod.f90
)
target_link_libraries(utils fortran_datetime eccodes_f90)

add_executable(decode_prepbufr_synop.exe "src/decode_prepbufr_synop.f90")
target_link_libraries(decode_prepbufr_synop.exe utils fortran_datetime fortran_container bufr Odb_fortran)

add_executable(decode_bufr_amdar.exe "src/decode_bufr_amdar.f90")
target_link_libraries(decode_bufr_amdar.exe utils fortran_datetime fortran_container eccodes_f90 Odb_fortran)

add_executable(decode_radar_x.exe "src/decode_radar_x.f90")
target_link_libraries(decode_radar_x.exe utils netcdff)

add_executable(decode_cimiss_synop.exe "src/decode_cimiss_synop.f90")
target_link_libraries(decode_cimiss_synop.exe utils fortran_container)
