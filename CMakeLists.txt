cmake_minimum_required (VERSION 2.6)
project (compiler-base)

include_directories ("${PROJECT_BINARY_DIR}")

add_definitions (-std=c++1z)
set (CMAKE_BUILD_TYPE Debug)
set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -fsanitize=address -fno-omit-frame-pointer")

# Force colored output for Ninja
if (CMAKE_GENERATOR MATCHES "Ninja")
  if (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    add_definitions (-fcolor-diagnostics)
  elseif (CMAKE_CXX_COMPILER_ID MATCHES "gcc")
    add_definitions (-fdiagnostics-color=always)
  endif ()
endif ()

set (CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

# libc++ options
set (LIBCXX_PATH "" CACHE PATH "doc")

if (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
  if (LIBCXX_PATH)
    include_directories ("${LIBCXX_PATH}/include/c++/v1")
    link_directories ("${LIBCXX_PATH}/lib")
    # Linking phase calls clang; supply it with -stdlib again
    set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -stdlib=libc++")
  endif ()
else ()
endif ()

# Creates executable target "compiler".
add_executable (compiler main.cc Source.cc Lexer.cc)

# Should be below the add_executable () line to avoid complaining about
# nonexistent target.
add_custom_command(TARGET compiler
  POST_BUILD
  COMMAND "dsymutil" $<TARGET_FILE:compiler>)
