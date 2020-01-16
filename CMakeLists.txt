cmake_minimum_required (VERSION 3.8)

project (qu LANGUAGES CXX)

add_executable (qc main.cc sema.cc parser.cc ast.cc lexer.cc error.cc source.cc
  format.cc)

if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE "DEBUG")
endif()

target_compile_features(qc PUBLIC cxx_std_17)
target_compile_options(qc PRIVATE -Wall -Wextra -fno-omit-frame-pointer
    -fdiagnostics-color=always "$<$<CONFIG:DEBUG>:-glldb>")
target_link_libraries(qc PRIVATE -fno-omit-frame-pointer)

# Comment this out to disable AddressSanitizer.
target_compile_options(qc PRIVATE
    "$<$<CONFIG:DEBUG>:-fsanitize=address,leak,undefined>")
target_link_libraries(qc PRIVATE
    "$<$<CONFIG:DEBUG>:-fsanitize=address,leak,undefined>")

set (CMAKE_EXPORT_COMPILE_COMMANDS ON)
