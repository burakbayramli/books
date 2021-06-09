function x = linsp2(x1,x2,n)
% linsp2  Generate a vector of equally spaced values, version 2
%
% Synopsis:  x = linsp2(x1,x2)
%            x = linsp2(x1,x2,n)
%
% Input:     x1, x2  = lower and upper limits of vector elements
%            n = (optional) number of elements to generate. Default: n=100
%
% Output:    x = vector of n equally spaced values in x1 <= x <= x2
if nargin<3,  n=100;  end
dx = (x2-x1)/(n-1);
x(1) = x1;
for k=2:n
  x(k) = x1 + (k-1)*dx;
end
