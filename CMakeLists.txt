cmake_minimum_required(VERSION 3.8)

project(qu LANGUAGES C)

add_executable(qc main.c sema.c parser.c lexer.c)
target_compile_features(qc PUBLIC c_std_99)
target_compile_options(qc PRIVATE -Wall -Wextra -fdiagnostics-color=always)
target_link_libraries(qc -fsanitize=address)

set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE "DEBUG")
endif()
