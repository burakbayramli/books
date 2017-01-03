function [u,x,t]=heat_exp(a,xf,T,it0,bx0,bxf,M,N)
%solve a u_xx = u_t  for 0<=x<=xf, 0<=t<=T
% Initial Condition: u(x,0)=it0(x) 
% Boundary Condition: u(0,t)=bx0(t), u(xf,t)=bxf(t)
% M = # of subintervals along x-axis
% N = # of subintervals along t-axis
dx= xf/M;  x= [0:M]'*dx; 
dt= T/N;  t= [0:N]*dt;
for i=1:M+1, u(i,1)= it0(x(i)); end
for n=1:N+1, u([1 M+1],n)= [bx0(t(n)); bxf(t(n))]; end
r= a*dt/dx/dx,  r1= 1-2*r; 
for k=1:N
   for i=2:M
      u(i,k+1)= r*(u(i+1,k)+u(i-1,k)) +r1*u(i,k); %Eq.(9.2-3)
   end
end
