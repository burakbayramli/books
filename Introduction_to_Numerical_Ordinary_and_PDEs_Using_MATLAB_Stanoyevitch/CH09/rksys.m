function [t,x]=rksys(vectorf,a,b,vecx,hstep)
%input variables: vectorf, a, b, vecx, hstep
%output variables: t, x
%Uses Runge Kutta to solve IVP of system of first order ODE:  x1'=f1, x2'=f2,...
%xn'=fn where vectorf is the vector valued function of x1, x2, ..., xn which 
% has components f1, f2, ..., fn.  The initial conditions x1(a)=x10, x2(a)=x20,...
%xn(a)=xn0 are stored in the vector vecx,  the final t-value is b, step size is
%hstep.
x(1,:)=vecx;
t=a:hstep:b;
[m nmax]=size(t);
for n=1:(nmax-1);
   k1=feval(vectorf,t(n),x(n,:));
   k2=feval(vectorf,t(n)+.5*hstep,x(n,:)+.5*hstep*k1);
   k3=feval(vectorf,t(n)+.5*hstep,x(n,:)+.5*hstep*k2);
   k4=feval(vectorf,t(n)+hstep,x(n,:)+hstep*k3);
   x(n+1,:)=x(n,:)+1/6*hstep*(k1+2*k2+2*k3+k4); 
end

   
   