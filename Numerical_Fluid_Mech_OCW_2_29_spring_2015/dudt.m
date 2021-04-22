function [f] = dudt(t,u)
% u(1) = u
% u(2) = x
% f(2) = dx/dt = u
% f(1) = du/dt=rho*Cd*pi*r/(2m)*(v^2-2uv+u^2)
%load dragpar;
rho=1000;
Cd=1;
m=5;
r=0.05;
fac=rho*Cd*pi*r^2/(2*m);
v=1;

f(1)=fac*(v^2-2*u(1)+u(1)^2);
f(2)=u(1);
f=f';