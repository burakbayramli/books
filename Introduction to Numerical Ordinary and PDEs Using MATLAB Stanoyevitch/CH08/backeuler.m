function [t, y] = backeuler(varf, a, b, y0, h)
% Performs the backward Euler method to solve an IVP
% Calls M-file 'newton' for rootfinding and uses symbolic capabilities 
% Input variables:  varf a function of two variables f(t,y) describing
% the ODE y' = f(t,y).  Can be an inline function or an M-file
% a, b = the left and right endpoints for the time interval of the IVP
% y0 the intial value y(a) given in the intial condition
% h = the step size to be used
% Output variables:  t = the vector of equally spaced time values for 
% the numerical solution, y = the corresponding vector of y coordinates.


syms  ys
t(1)=a;, y(1)=y0;
nmax=ceil((b-a)/h);
for n=1:nmax
    t(n+1)=t(n)+h;
    g=inline(char(ys-y(n)-h*varf(t(n+1),ys)), 'ys');
    gp=diff(g(ys));
    gprime=inline(vectorize(char(gp)), 'ys');
    y(n+1)=newton(g,gprime,y(n));
end






   