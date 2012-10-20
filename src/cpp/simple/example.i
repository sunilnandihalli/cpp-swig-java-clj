/* File : example.i */
%module simple_module

%inline %{
extern int    gcd(int x, int y);
extern double Foo;
%}
