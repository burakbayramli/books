function [t,x]= bvp2_shoot(f,t0,tf,x0,xf,N,tol,kmax,Kg)
%To solve BVP2: [x1,x2]'=f(t,x1,x2) with x1(t0)=x0, x1(tf)=xf
% by the shooting method
if nargin<9, Kg= 0; end
if nargin<8, kmax= 10; end
if nargin<7, tol= 1e-8; end
if nargin<6, N=100; end
dx0(1)= (xf-x0)/(tf-t0);
%dx0(1)= 0;
%[t,x]= ode23s(f,[t0 tf],[x0 dx0(1)]);
[t,x]= ode_RK4(f,[t0 tf],[x0 dx0(1)],N);
if Kg>0, plot(t,x(:,1)), hold on; end
e(1)=x(end,1)-xf;
dx0(2)= dx0(1)-0.1*sign(e(1));
for k=2: kmax-1
   [t,x]= ode_RK4(f,[t0 tf],[x0 dx0(k)],N);
   if k<Kg, pause, plot(t,x(:,1)); end
   e(k)=x(end,1)-xf; %difference between the resulting final value and the desired one
   ddx= dx0(k)-dx0(k-1);
   if abs(e(k))<tol|abs(ddx)<tol, break; end
   deddx= (e(k)-e(k-1))/ddx;           
   dx0(k+1)= dx0(k)-e(k)/deddx; %move by secant method
end