%module misc_utils
%{
#include "stuff/stuff.h"
#include <vector>
%}
%include "stuff/stuff.h"
%include std_vector.i
%template(complexInt) complexT<int>;
%template(complexDouble) complexT<double>;
%template(vecInt) std::vector<int>;

