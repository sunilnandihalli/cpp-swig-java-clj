add_library(cpp stuff.cpp)
install (TARGETS cpp DESTINATION bin)
install (FILES stuff.h DESTINATION include)
