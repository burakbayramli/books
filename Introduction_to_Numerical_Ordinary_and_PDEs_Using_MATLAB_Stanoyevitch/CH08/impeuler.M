function [t,y]=impeuler(f,a,b,y0,hstep)
% input variables:  f, a, b, y0, hstep
% output variables:  t, y
% f is a function of two variables f(t,y).  The program will
% apply Imp. Euler to solve the IVP:  (DE):  y'=f(t,y), (IC)
% y(a)=y0 on the t-interval [a,b] with step size hstep.  The output
% will be a vector of t's and corresponding y's
t(1)=a;, y(1)=y0;
nmax=ceil((b-a)/hstep);
for n=1:nmax
   t(n+1)=t(n)+hstep;
   y(n+1)=y(n)+.5*hstep*(feval(f,t(n),y(n))...
   +feval(f,t(n+1),y(n)...
   +hstep*feval(f,t(n),y(n))));
end

   