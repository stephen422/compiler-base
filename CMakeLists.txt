cmake_minimum_required (VERSION 2.6)
project (compiler-base)

enable_testing ()

include_directories ("${PROJECT_BINARY_DIR}")

add_definitions (-std=c++17)
set (CMAKE_BUILD_TYPE Debug)
set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -fno-omit-frame-pointer")

# Compiler invocation switch
option (COMPILER_VERBOSE "Print verbose compiler invocation" OFF)

if (COMPILER_VERBOSE)
  set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -v")
endif ()

# Force colored output for Ninja
if (CMAKE_GENERATOR MATCHES "Ninja")
  if (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    add_definitions (-fcolor-diagnostics)
  elseif (CMAKE_CXX_COMPILER_ID MATCHES "GNU")
    add_definitions (-fdiagnostics-color=always)
  endif ()
endif ()

set (CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

# libc++ options
set (LIBCXX_PATH "" CACHE PATH "doc")

if (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
  if (LIBCXX_PATH)
    include_directories ("${LIBCXX_PATH}/include/c++/v1")
    # link_directories ("${LIBCXX_PATH}/lib")
    # Linking phase calls clang; supply it with -stdlib again
    set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -stdlib=libc++")
    set (CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -Wl,-rpath=${LIBCXX_PATH}")
  endif ()
else ()
endif ()

# Creates executable target "compiler".
add_executable (compiler main.cc Source.cc Lexer.cc)

if (APPLE)
  # Should be below the add_executable () line to avoid complaining about
  # nonexistent target.
  add_custom_command(TARGET compiler
    POST_BUILD
    COMMAND "dsymutil" $<TARGET_FILE:compiler>)
endif ()

### Testing

# Creates the test target.
add_executable (compiler-test EXCLUDE_FROM_ALL test.cc Source.cc Lexer.cc)

# Use "env CTEST_OUTPUT_ON_FAILURE=1 ninja test" to see output on fail.
add_test (NAME test COMMAND compiler-test "--use-colour" "yes")

# Workaround of the cmake bug of 'test' not depending on its test target [1].
# Use "ninja check" to rebuild the target on demand and then run the test.
#
# This also solves the problem of CTest not outputting anything even on test
# failure, which in case of "ninja test" should be solved by explicitly setting
# env $CTEST_OUTPUT_ON_FAILURE to 1.
#
# [1] https://stackoverflow.com/questions/733475/cmake-ctest-make-test-doesnt-build-tests

add_custom_target (check COMMAND ${CMAKE_CTEST_COMMAND} "--output-on-failure"
  DEPENDS compiler-test)
