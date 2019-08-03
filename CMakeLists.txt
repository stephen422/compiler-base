cmake_minimum_required(VERSION 3.8)

project(qu LANGUAGES C)

add_executable(qc main.c sema.c parser.c lexer.c)
target_compile_features(qc PUBLIC c_std_99)
target_compile_options(qc PRIVATE -Wall -Wextra -fdiagnostics-color=always)
