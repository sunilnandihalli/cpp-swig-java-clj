cmake_minimum_required(VERSION 2.6)
project(swig_java)
set(CMAKE_BUILD_TYPE DEBUG)
set(CMAKE_C_FLAGS "-O0 -ggdb3")
set(CMAKE_C_FLAGS_DEBUG "-O0 -ggdb3")
set(CMAKE_C_FLAGS_RELEASE "-O0 -ggdb3")
set(CMAKE_CXX_FLAGS "-O0 -ggdb3")
set(CMAKE_CXX_FLAGS_DEBUG "-O0 -ggdb3")
set(CMAKE_CXX_FLAGS_RELEASE "-O0 -ggdb3")
SUBDIRS(cpp swig_files)
add_library(misc_utils_java)
target_link_libraries(misc_utils_java cpp swig_files)
install(TARGETS misc_utils_java DESTINATION ${PROJECT_SOURCE_DIR}/../cpp_lib)