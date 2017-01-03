%nm6p08a.m: to solve BVP2 with mixed boundary conditions% x"=(2t/t^2+1)*x' -2/(t^2+1)*x +t^2+1 
%  with x(0)+6x'(0)=0, x'(1)+x(1)=0
%shooting method
f=inline('[x(2); 2*(t*x(2)-x(1))./(t.^2+1)+(t.^2+1)]','t','x');
t0=0; tf=1; N=100; tol=1e-8; kmax= 10;   
c0=[1 6 0]; cf=[1 1 0];%coefficient vectors of boundary condition 
[tt,x_sh]=bvp2mm_shoot(f,t0,tf,c0,cf,N,tol,kmax);
plot(tt,x_sh(:,1),'b')
