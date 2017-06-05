cmake_minimum_required (VERSION 2.6)
project (compiler-base)

include_directories ("${PROJECT_BINARY_DIR}")

add_definitions (-std=c++1z)
set (CMAKE_BUILD_TYPE Debug)

# Force colored output for Ninja
if (CMAKE_GENERATOR MATCHES "Ninja")
  add_definitions (-fcolor-diagnostics)
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

add_executable (compiler main.cc Source.cc)
