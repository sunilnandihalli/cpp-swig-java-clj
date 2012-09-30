
add_library(cpp SHARED stuff.cpp)

install (TARGETS cpp DESTINATION ${PROJECT_BINARY_DIR}/../../cpp_lib)
install (FILES stuff.h DESTINATION ${PROJECT_BINARY_DIR}/../../cpp_include)

