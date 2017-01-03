function r = Trapezoidal_vec(a, b, f, n)
% TRAPEZOIDAL Numerical integration from a to b 
% by the Trapezoidal rule
f = fcnchk(f);
h = (b-a)/n;
x = [a:h:b];
v = feval(f, x);
r = h*(sum(v) - 0.5*(v(1) + v(length(v))));
