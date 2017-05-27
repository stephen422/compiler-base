cmake_minimum_required (VERSION 2.6)
project (compiler-base)

include_directories ("${PROJECT_BINARY_DIR}")

if ("${CMAKE_CXX_COMPILER}" STREQUAL "Clang")
  message ("Hi clang!")
else ()
  message ("No clang")
endif ()

# Enable C++1z standards
add_definitions (-std=c++1z)

set (CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

add_executable (compiler Source.cc)
