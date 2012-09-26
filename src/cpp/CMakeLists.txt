add_library(cpp SHARED stuff.cpp)
install (TARGETS cpp DESTINATION bin)
install (FILES stuff.h DESTINATION include)
