function [t, y] = adamsbash5(varf, a, b, y0, h)
% Performs the Adams-Bashforth fifth order scheme to solve an IVP
% Calls on fifth order Runge-Kutta scheme (rk5) to create the seed
% iterates.
% Input variables:  varf a function of two variables f(t,y) describing
% the ODE y' = f(t,y).  Can be an inline function or an M-file
% a, b = the left and right endpoints for the time interval of the IVP
% y0 the intial value y(a) given in the intial condition
% h = the step size to be used
% Output variables:  t = the vector of equally spaced time values for 
% the numerical solution, y = the corresponding vector of y coordinates.

nmax=ceil((b-a)/h);
%first form the seed iterates using single step Runge-Kutta
[t,y]=rk5(varf,a,a+4*h,y0,h);

for n=5:nmax
 t(n+1)=t(n)+h;
  y(n+1)=y(n)+h/720*(1901*feval(varf,t(n),y(n))-2774*feval(varf,t(n-1),y(n-1))...
    +2616*feval(varf,t(n-2),y(n-2))-1274*feval(varf,t(n-3),y(n-3))+251*feval(varf,t(n-4),y(n-4)));
end

   