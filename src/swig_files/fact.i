/* File: example.i */
%module test
%{
#include "stuff.h"
#include <vector>
  %}
%include "stuff.h"
%include std_vector.i
%template(complexInt) complexT<int>;
%template(complexDouble) complexT<double>;
%template(vecInt) std::vector<int>;

