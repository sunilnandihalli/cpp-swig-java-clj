%module misc_utils
%{
#include "misc_utils.hpp"
#include <vector>
%}
%include "misc_utils.hpp"
%include std_vector.i
%template(complexInt) misc_utils::complexT<int>;
%template(complexDouble) misc_utils::complexT<double>;
%template(vecInt) std::vector<int>;

