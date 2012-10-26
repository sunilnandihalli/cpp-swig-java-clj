/* File : example.i */
%module(directors="1") callback_module
%{
#include "example.h"
%}

%include "std_string.i"


%pragma(java) jniclasscode=%{
   static {
     try { System.loadLibrary("callback_java");
     }
     catch (RuntimeException e) {
       System.out.println("Failed to load the C++ libraries during SWIG module initialisation");
       e.printStackTrace();
     }
   }
%}

/* turn on director wrapping Callback */
%feature("director") Callback;

%include "example.h"

