%module misc_utils
%{
#include "stuff/stuff.hpp"
#include <vector>
%}
%include "stuff/stuff.hpp"
%include std_vector.i
%template(complexInt) complexT<int>;
%template(complexDouble) complexT<double>;
%template(vecInt) std::vector<int>;

