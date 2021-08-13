function y=f2a(x,a)

%y=f2a(x,a)
%
%  This function implements the function f2(x)=sin(a*x).  The parameter
%  a is optional; if it is not provided, it is taken to be 1.

if nargin<2
   a=1;
end

y=sin(a*x);

