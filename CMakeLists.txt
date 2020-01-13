cmake_minimum_required (VERSION 3.8)

project (qu LANGUAGES CXX)

add_executable (qc main.cc sema.cc parser.cc ast.cc lexer.cc source.cc
    format.cc)
target_compile_features(qc PUBLIC cxx_std_17)
target_compile_options(qc PRIVATE -Wall -Wextra -fno-omit-frame-pointer
    -fsanitize=address -fdiagnostics-color=always)
target_link_libraries(qc PRIVATE -fno-omit-frame-pointer -fsanitize=address)

set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE "DEBUG")
endif()
