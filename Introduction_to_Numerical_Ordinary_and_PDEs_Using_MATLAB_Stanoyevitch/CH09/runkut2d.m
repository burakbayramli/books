function [t,x,y]=runkut2d(f,g,a,b,x0,y0,hstep)
% This M-file performs the Runge-Kutta method to solve a two dimensional system of form:
% Dx(t)= f(t,x,y), x(a) = x0,  Dy(t) = g(t,x,y), y(a) = y0, on the interval
% a <= t <= b.
% Input variables:  f and g inline (or M-files) for the derivatives of the 
% unknown functions x(t) and y(t).  These must be specified as functions of 
% the three variables:  t, x, and y (in this order)
% a and b:  endpoints for the time interval on which the solution is sought
% x0, and y0, initial conditions for the unknown functions at t = a
% hstep, the step size (any positive number)
% output variables are three vectors of the same size: t, x and y, for the 
% numerical solution. 
t=a:hstep:b;,x(1)=x0;, y(1)=y0;
[m nmax]=size(t);
for n=1:(nmax-1)
   k1x=feval(f,t(n),x(n),y(n));
   k1y=feval(g,t(n),x(n),y(n));
   k2x=feval(f,t(n)+.5*hstep,x(n)+.5*hstep*k1x,y(n)+.5*hstep*k1y);
   k2y=feval(g,t(n)+.5*hstep,x(n)+.5*hstep*k1x,y(n)+.5*hstep*k1y);
   k3x=feval(f,t(n)+.5*hstep,x(n)+.5*hstep*k2x,y(n)+.5*hstep*k2y);
   k3y=feval(g,t(n)+.5*hstep,x(n)+.5*hstep*k2x,y(n)+.5*hstep*k2y);
   k4x=feval(f,t(n)+hstep,x(n)+hstep*k3x,y(n)+hstep*k3y);
   k4y=feval(g,t(n)+hstep,x(n)+hstep*k3x,y(n)+hstep*k3y);
   x(n+1)=x(n)+1/6*hstep*(k1x+2*k2x+2*k3x+k4x);   
   y(n+1)=y(n)+1/6*hstep*(k1y+2*k2y+2*k3y+k4y);   
end


   