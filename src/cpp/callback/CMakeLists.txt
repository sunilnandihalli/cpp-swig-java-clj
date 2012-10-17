include_directories(. ${JNI_INCLUDE_DIRS}) 
set(PKG_NAME "callback")
set(CMAKE_SWIG_FLAGS -package ${PKG_NAME})
set(CMAKE_SWIG_OUTDIR "${CMAKE_SWIG_OUTDIR}/${PKG_NAME}")
file(MAKE_DIRECTORY ${CMAKE_SWIG_OUTDIR})
set_source_files_properties(example.i PROPERTIES CPLUSPLUS ON)
swig_add_module(callback_java java example.i) 
swig_link_libraries(callback_java ${JNI_LIBRARIES} ${callback_java})           
install(TARGETS callback_java DESTINATION ${CMAKE_SOURCE_DIR}/../cpp_lib)
add_custom_target(${PKG_NAME}_runme ALL
   echo "package ${PKG_NAME};" | cat - ${CMAKE_CURRENT_SOURCE_DIR}/runme.java > ${CMAKE_SWIG_OUTDIR}/runme.java)

  
#set_source_files_properties(example.i PROPERTIES SWIG_FLAGS "-package\ callback")
