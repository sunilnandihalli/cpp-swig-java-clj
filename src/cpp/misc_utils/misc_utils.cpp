#include "misc_utils.hpp"
namespace misc_utils {
  int fact(int n) {
    return n<=1?1:n*fact(n-1);
  }
  
  complex::complex(double _re,double _im):re(_re),im(_im) {
  }
  complex complex::operator+(const complex& b) {
    return complex(re+b.re,im+b.im);
  }
  
  complex complex::add(const complex& a) {
    return complex(re+a.re,im+a.im);
  }
}
