/* File : example.i */
%module jclass_module

%{
#include "example.h"
%}

/* Let's just grab the original header file here */
%include "example.h"

