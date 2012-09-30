
add_library(cpp SHARED stuff.cpp)

install (TARGETS cpp DESTINATION ${PROJECT_SOURCE_DIR}/../cpp_lib)
install (FILES stuff.h DESTINATION ${PROJECT_SOURCE_DIR}/../cpp_include)

