function [u,x,t]=wave(a,xf,T,it0,i1t0,bx0,bxf,M,N)
%solve a u_xx = u_tt  for 0<=x<=xf, 0<=t<=T
% Initial Condition: u(x,0) = it0(x), u_t(x,0) = i1t0(x) 
% Boundary Condition: u(0,t) = bx0(t), u(xf,t)=bxf(t)
% M = # of subintervals along x-axis
% N = # of subintervals along t-axis
dx= xf/M;  x= [0:M]'*dx; 
dt= T/N;  t= [0:N]*dt;
for i=1:M+1, u(i,1)= it0(x(i)); end
for k=1:N+1
   u([1 M+1],k)= [bx0(t(k)); bxf(t(k))];
end
r= a*(dt/dx)^2;  r1= r/2;  r2= 2*(1-r);
u(2:M,2)= r1*u(1:M-1,1) +(1-r)*u(2:M,1) +r1*u(3:M+1,1) ... 
+dt*i1t0(x(2:M)); %Eq.(9.3-6)
for k=3:N+1  
  u(2:M,k)= r*u(1:M-1,k-1) +r2*u(2:M,k-1) +r*u(3:M+1,k-1)...
-u(2:M,k-2); %Eq.(9.3-3)
end
