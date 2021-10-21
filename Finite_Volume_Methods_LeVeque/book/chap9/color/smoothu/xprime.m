function xprime = xprime(t,x)
% this should be -u(x) and is used to trace characteristics backwards in 
% time to determine the exact solution of the variable-coefficient color
% equation.  Called from computetrue.m

xprime = -(1 - 0.5 * sin(2*pi*x));

