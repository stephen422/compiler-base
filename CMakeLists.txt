cmake_minimum_required (VERSION 3.8)

project (compiler-base LANGUAGES CXX)

add_executable (cmp main.cc driver.cc sema.cc parser.cc ast.cc lexer.cc error.cc source.cc
  format.cc)
add_executable (cmp-check test_main.cc test.cc driver.cc sema.cc parser.cc ast.cc lexer.cc error.cc source.cc
  format.cc)

if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE "DEBUG")
endif()

list(APPEND MY_COMPILE_FLAGS -Wall -Wextra -Wno-unused-parameter
  -fno-omit-frame-pointer)
list(APPEND MY_LINK_FLAGS -fno-omit-frame-pointer)

if (CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
  list(APPEND MY_COMPILE_FLAGS -fdiagnostics-color=always $<$<CONFIG:DEBUG>:-ggdb>)

  if(UNIX AND NOT APPLE)
    # Link to libc++
    # list(APPEND MY_COMPILE_FLAGS -stdlib=libc++)
    # list(APPEND MY_LINK_FLAGS -stdlib=libc++ -rtlib=compiler-rt
    #   -unwindlib=libunwind -Wl,-rpath=$ENV{HOME}/build/llvm/lib)

    # Selectively use LLD on Linux
    list(APPEND MY_LINK_FLAGS -fuse-ld=lld)
  else()
    list(APPEND MY_LINK_FLAGS
      -Wl,-L$ENV{HOME}/build/llvm/lib -Wl,-rpath,$ENV{HOME}/build/llvm/lib)
  endif()
elseif (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
  list(APPEND MY_COMPILE_FLAGS -fdiagnostics-color=always $<$<CONFIG:DEBUG>:-ggdb>)
endif()

if(UNIX AND NOT APPLE)
  list(APPEND MY_COMPILE_FLAGS
    "$<$<CONFIG:DEBUG>:-fsanitize=address,leak,undefined>")
  list(APPEND MY_LINK_FLAGS
    "$<$<CONFIG:DEBUG>:-fsanitize=address,leak,undefined>")
else()
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
