function I = trapezoid(fun,a,b,npanel)
% trapezoid  Composite trapezoid rule
%
% Synopsis:  I = trapezoid(fun,a,b,npanel)
%
% Input:     fun    = (string) name of m-file that evaluates f(x)
%            a, b   = lower and upper limits of the integral
%            npanel = number of panels to use in the integration
%                     Total number of nodes = npanel + 1
%
% Output:    I = approximate value of the integral from a to b of f(x)*dx

n = npanel + 1;      %  total number of nodes
h = (b-a)/(n-1);     %  stepsize
x = a:h:b;           %  divide the interval
f = feval(fun,x);    %  evaluate integrand

I = h * ( 0.5*f(1) + sum(f(2:n-1)) + 0.5*f(n) );
