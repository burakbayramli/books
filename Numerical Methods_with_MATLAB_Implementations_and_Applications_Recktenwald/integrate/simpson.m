function I = simpson(fun,a,b,npanel)
% simpson    Composite Simpson's rule
%
% Synopsis:  I = simpson(fun,a,b,npanel)
%
% Input:     fun    = (string) name of m-file that evaluates f(x)
%            a, b   = lower and upper limits of the integral
%            npanel = number of panels to use in the integration
%                     Total number of nodes = 2*npanel + 1
%
% Output:    I = approximate value of the integral from a to b of f(x)*dx

n = 2*npanel + 1;    %  total number of nodes
h = (b-a)/(n-1);     %  stepsize
x = a:h:b;           %  divide the interval
f = feval(fun,x);    %  evaluate integrand

I = (h/3)*( f(1) + 4*sum(f(2:2:n-1)) + 2*sum(f(3:2:n-2)) + f(n) );
%           f(a)         f_even              f_odd         f(b)
