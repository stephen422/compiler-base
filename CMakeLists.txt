cmake_minimum_required (VERSION 3.8)

project (compiler-base LANGUAGES CXX)

add_executable (cmp main.cc sema.cc parser.cc ast.cc lexer.cc error.cc source.cc
  format.cc)

if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE "DEBUG")
endif()

target_compile_features(cmp PUBLIC cxx_std_17)
target_compile_options(cmp PRIVATE -Wall -Wextra -Wno-unused-parameter
  -fno-omit-frame-pointer -fdiagnostics-color=always "$<$<CONFIG:DEBUG>:-glldb>")
target_link_libraries(cmp PRIVATE -fno-omit-frame-pointer)

# Selectively use LLD on Linux
if(UNIX AND NOT APPLE)
    target_compile_options(cmp PRIVATE -stdlib=libc++)
    target_link_libraries(cmp PRIVATE -stdlib=libc++ -rtlib=compiler-rt -unwindlib=libunwind -Wl,-rpath=$ENV{HOME}/build/llvm/lib)
    target_link_libraries(cmp PRIVATE -fuse-ld=lld)

    target_compile_options(cmp PRIVATE
      "$<$<CONFIG:DEBUG>:-fsanitize=address,leak,undefined>")
    target_link_libraries(cmp PRIVATE
      "$<$<CONFIG:DEBUG>:-fsanitize=address,leak,undefined>")
else()
    # LeakSanitizer does not work on Darwin
    target_compile_options(cmp PRIVATE
      "$<$<CONFIG:DEBUG>:-fsanitize=address,undefined>")
    target_link_libraries(cmp PRIVATE
      "$<$<CONFIG:DEBUG>:-fsanitize=address,undefined>")
endif()

set (CMAKE_EXPORT_COMPILE_COMMANDS ON)
