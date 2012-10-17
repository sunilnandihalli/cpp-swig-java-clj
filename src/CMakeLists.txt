cmake_minimum_required(VERSION 2.6)
project(swig_java)
find_package(SWIG REQUIRED)
include(${SWIG_USE_FILE}) 
find_package(JNI REQUIRED) 

set(CMAKE_CXX_FLAGS "-fPIC")
set(CPP_SOURCES "")
set(CPP_INCLUDE_DIRS "")
set(JAVA_SOURCES "")
set(SWIG_INTERFACE_FILES "")
set(CMAKE_SWIG_FLAGS "")                                                                                                                 
set(CMAKE_SWIG_OUTDIR "${PROJECT_SOURCE_DIR}/../gen_java_src/")
file(MAKE_DIRECTORY ${CMAKE_SWIG_OUTDIR}) 
add_subdirectory(swig_files)
add_subdirectory(cpp)
add_subdirectory(java)

message("CPP_SOURCES : '${CPP_SOURCES}'")
message("CPP_INCLUDE_DIRS : '${CPP_INCLUDE_DIRS}'")
message("JAVA_SOURCES : '${JAVA_SOURCES}'")
message("SWIG_INTERFACE_FILES : '${SWIG_INTERFACE_FILES}'")

include_directories(${CPP_INCLUDE_DIRS} ${JNI_INCLUDE_DIRS})
add_library(cpp_lib ${CPP_SOURCES})


set_source_files_properties(${SWIG_INTERFACE_FILES} PROPERTIES CPLUSPLUS ON)

swig_add_module(java_interop java ${SWIG_INTERFACE_FILES})
swig_link_libraries(java_interop ${JNI_LIBRARIES} cpp_lib)
install(TARGETS java_interop DESTINATION ${CMAKE_SOURCE_DIR}/../cpp_lib)

