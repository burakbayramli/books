function [t,y]=runkut(f,a,b,y0,hstep)
% input variables:  f, a, b, y0, hstep
% output variables:  t, y
% f is a function of two variables f(t,y).  The program will
% apply Runge-Kutta to solve the IVP:  (DE):  y'=f(t,y), (IC)
% y(a)=y0 on the t-interval [a,b] with step size hstep.  The output
% will be a vector of t's and corresponding y's
t(1)=a;, y(1)=y0;
nmax=ceil((b-a)/hstep);
for n=1:nmax
   t(n+1)=t(n)+hstep;
   k1=feval(f,t(n),y(n));
   k2=feval(f,t(n)+.5*hstep,y(n)+.5*hstep*k1);
   k3=feval(f,t(n)+.5*hstep,y(n)+.5*hstep*k2);
   k4=feval(f,t(n)+hstep,y(n)+hstep*k3);
   y(n+1)=y(n)+1/6*hstep*(k1+2*k2+2*k3+k4);   
end


   