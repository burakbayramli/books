function [t,x,y]=euler2d(f,g,a,b,x0,y0,hstep)
% input variables:  f, g a, b, y0, hstep
% output variables:  t, x, y
% f and g are functions of three variables (t,x,y).  
% The program will apply Euler's method to solve the IVP:
% (DE's):  x'=f(t,x,y), y'=g(t,x,y)  (IC's) x(a)=x0
% y(a)=y0 on the t-interval [a,b] with step size hstep.  The output
% will be a vector of t's and corresponding  x's and y's
x(1)=x0;, y(1)=y0;
t=a:hstep:b;
[m nmax]=size(t);
for n=1:nmax-1 %This will make t have same length as x,y   
   x(n+1)=x(n)+hstep*feval(f,t(n),x(n),y(n)); 
   y(n+1)=y(n)+hstep*feval(g,t(n),x(n),y(n));
end