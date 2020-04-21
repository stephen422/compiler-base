cmake_minimum_required (VERSION 3.8)

project (compiler-base LANGUAGES CXX)

add_executable (cmp main.cc driver.cc sema.cc parser.cc ast.cc lexer.cc error.cc source.cc
  format.cc)
add_executable (cmp-check test.cc driver.cc sema.cc parser.cc ast.cc lexer.cc error.cc source.cc
  format.cc)

if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE "DEBUG")
endif()

list(APPEND MY_COMPILE_FLAGS -Wall -Wextra -Wno-unused-parameter
  -fno-omit-frame-pointer -fdiagnostics-color=always $<$<CONFIG:DEBUG>:-glldb>)
list(APPEND MY_LINK_FLAGS -fno-omit-frame-pointer)

# Selectively use LLD on Linux
if(UNIX AND NOT APPLE)
  list(APPEND MY_COMPILE_FLAGS -stdlib=libc++)
  list(APPEND MY_LINK_FLAGS -fuse-ld=lld -stdlib=libc++ -rtlib=compiler-rt
    -unwindlib=libunwind -Wl,-rpath=$ENV{HOME}/build/llvm/lib)

  list(APPEND MY_COMPILE_FLAGS
    "$<$<CONFIG:DEBUG>:-fsanitize=address,leak,undefined>")
  list(APPEND MY_LINK_FLAGS
    "$<$<CONFIG:DEBUG>:-fsanitize=address,leak,undefined>")
else()
  list(APPEND MY_LINK_FLAGS
    -Wl,-L$ENV{HOME}/build/llvm/lib -Wl,-rpath,$ENV{HOME}/build/llvm/lib)

  # LeakSanitizer does not work on Darwin
  list(APPEND MY_COMPILE_FLAGS
    "$<$<CONFIG:DEBUG>:-fsanitize=address,undefined>")
  list(APPEND MY_LINK_FLAGS
    "$<$<CONFIG:DEBUG>:-fsanitize=address,undefined>")
endif()

target_compile_features(cmp PUBLIC cxx_std_17)
target_compile_options(cmp PRIVATE ${MY_COMPILE_FLAGS})
target_link_options(cmp PRIVATE ${MY_LINK_FLAGS})

target_compile_features(cmp-check PUBLIC cxx_std_17)
target_compile_options(cmp-check PRIVATE ${MY_COMPILE_FLAGS})
target_compile_definitions(cmp-check PRIVATE CATCH_CONFIG_COLOUR_ANSI)
target_link_options(cmp-check PRIVATE ${MY_LINK_FLAGS})

add_custom_target(check COMMAND ./cmp-check --use-colour yes DEPENDS cmp-check)

set (CMAKE_EXPORT_COMPILE_COMMANDS ON)
