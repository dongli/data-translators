cmake_minimum_required(VERSION 2.8)

project(fortran_regex LANGUAGES CXX Fortran)

add_library(fortran_regex regex_master.cpp regex.f90)
if (APPLE)
  target_link_libraries(fortran_regex c++)
elseif (UNIX)
  target_link_libraries(fortran_regex stdc++)
endif ()
