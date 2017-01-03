function I = trapzDat(x,f)
% trapzDat  Composite trapezoid rule for arbitrarily spaced discrete data
%
% Synopsis:  I = trapzDat(x,f)
%
% Input:     x = vector of independent variable data.  Assumed
%                to be unequally spaced
%            f = vector discrete function values to be integrated.  
%
% Output:    I = integral of f with respect to x

n = length(f);
if length(x)~=n,  error('Dimensions of x and f are incompatible');  end

dx   = diff(x);                  %  vector of x(i+1)-x(i) values
avef = f(1:n-1) + 0.5*diff(f);   %  vector of average f values
I = sum(avef.*dx);               %  avef(1)*dx(1) + avef(2)*dx(2) + ...
