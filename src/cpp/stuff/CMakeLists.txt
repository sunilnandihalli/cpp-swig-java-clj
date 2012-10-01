
add_library(stuff SHARED stuff.cpp)
install (TARGETS stuff DESTINATION ${PROJECT_SOURCE_DIR}/../cpp_lib)
install (FILES stuff.h DESTINATION ${PROJECT_SOURCE_DIR}/../cpp_include/stuff)

