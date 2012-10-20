#ifndef MISC_UTILS_HPP
#define MISC_UTILS_HPP
namespace misc_utils {
  int fact(int n);
  class complex {
  public:
    double re,im;
    complex(double _re,double _im);
#ifdef SWIG
    %rename(add_op) operator+(const complex& a);
#endif
    complex operator+(const complex& b);
    complex add(const complex& a);
  };
  
  template <class T>
  struct complexT {
    T re,im;
    complexT(T _re,T _im):re(_re),im(_im) {
    }
#ifdef SWIG 
    %rename(add_op) operator+(const complexT<T>& a);
#endif
    complexT<T> operator+(const complexT<T>& a) {
      return complexT<T>(re+a.re,im+a.im);
    }
    complexT<T> add(const complexT<T>& a) {
      return complexT<T>(re+a.re,im+a.im);
    }
  };
}
#endif
