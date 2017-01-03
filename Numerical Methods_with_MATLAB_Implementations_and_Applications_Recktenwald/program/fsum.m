function s = fsum(fun,a,b,n)
% fsum  Computes sum of f(x) values at n points in  a <= x <= b
%
% Synopsis:   s = fsum(fun,a,b,n)
%
% Input:  fun = (string) name of the function, f(x), to be evaluated
%         a,b = endpoints of the interval
%         n   = number of points in the interval
%
% Output:  s = sum of f(x) at n discrete points in the interval

x = linspace(a,b,n);     %  create points in the interval
y = feval(fun,x);        %  evaluate function at sample points
s = sum(y);              %  compute the sum
