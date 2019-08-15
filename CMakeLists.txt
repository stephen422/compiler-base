cmake_minimum_required (VERSION 3.8)

project (qu LANGUAGES CXX)

add_executable (qc main.cpp sema.cpp parser.cpp ast.cpp lexer.cpp source.cpp)
target_compile_features(qc PUBLIC cxx_std_17)
target_compile_options(qc PRIVATE -Wall -Wextra -fno-omit-frame-pointer
    -fsanitize=address -fdiagnostics-color=always)
target_link_libraries(qc -fno-omit-frame-pointer -fsanitize=address)
