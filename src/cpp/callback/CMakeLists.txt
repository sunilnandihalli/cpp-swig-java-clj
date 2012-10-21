set_source_files_properties(example.i PROPERTIES CPLUSPLUS ON)
add_swig_java_wrapper(callback example.cxx)
  
#set_source_files_properties(example.i PROPERTIES SWIG_FLAGS "-package\ callback")
