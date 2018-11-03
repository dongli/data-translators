cmake_minimum_required(VERSION 2.8)

project(fortran_regex LANGUAGES CXX Fortran)

set (CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/.mods")

add_library(fortran_regex regex_master.cpp regex.f90)
target_link_libraries(fortran_regex stdc++)
