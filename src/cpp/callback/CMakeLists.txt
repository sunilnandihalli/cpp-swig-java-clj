set_source_files_properties(example.i PROPERTIES CPLUSPLUS ON)                                                                           
include_directories(${JNI_INCLUDE_DIRS}) 
swig_add_module(callback_java java example.i) 
swig_link_libraries(callback_java ${JNI_LIBRARIES} ${swig_files})           

