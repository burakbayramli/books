function [t,y]=eulermeth(f,a,b,y0,hstep)
% M-file for applying Euler’s method to solve the initial value 
% problem:  (DE) y’=f(t,y), (IC) y(a) = y0, on the t-interval [a,b] 
% with step size hstep.  The output will be a vector of t's and 
% corresponding y's
% input variables:  f, a, b, y0, hstep
% output variables:  t, y
% f is a function of two variables f(t,y)
% y(a)=y0 
t(1)=a; y(1)=y0;
nmax=ceil((b-a)/hstep);
for n=1:nmax
   t(n+1)=t(n)+hstep;
   y(n+1)=y(n)+hstep*feval(f,t(n),y(n));
end
