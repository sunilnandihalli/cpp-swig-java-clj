project(misc_utils)
add_library(misc_utils misc_utils.cpp)
install (TARGETS misc_utils DESTINATION ${PROJECT_SOURCE_DIR}/../cpp_lib)
install (FILES misc_utils.hpp DESTINATION ${PROJECT_SOURCE_DIR}/../cpp_include/misc_utils)

