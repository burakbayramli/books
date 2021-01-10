% Main.m
%LBM- 2-D2Q9, Flow in a channel with step, note that c2=1/3, w9=4/9,
% w1-4=1/9, and w5-w8, 1/36
clear
nx=501;ny=81;
f=zeros(nx,ny,9);feq=zeros(nx,ny,9);
u=zeros(nx,ny);v=zeros(nx,ny);
rho=ones(nx,ny);x=zeros(nx);y=zeros(ny);
Tm=zeros(nx);w(9)=zeros; Tvm=zeros(nx);
w=[1/9 1/9 1/9 1/9 1/36 1/36 1/36 1/36 4/9];
cx = [1 0 -1 0 1 -1 -1 1 0];
cy = [0 1 0 -1 1 1 -1 -1 0];
c2=1./3.;
dx=1.0;dy=1.0;
xl=(nx-1)/(ny-1); yl=1.0;
dx=xl/(nx-1);
dy=yl/(ny-1);
x=(0:dx:xl);
y=(0:dy:yl);
uo=0.1;
alpha=0.01;
Re=uo*(ny-1)/alpha
omega=1./(3.*alpha+0.5);
count=0; tol=1.0e-4; error=10.;erso=0.0;
%setting velocity

for j=2:ny-1

u(1,j)=uo;

end

%Main Loop

while error>tol

% Collitions
[f]=collition(nx,ny,u,v,cx,cy,omega,f,rho,w);

% Streaming:
[f]=stream(f);

% End of streaming
%Boundary condition:
[f]=boundary(nx,ny,f,uo,rho);
%Obsticale
[f]=obstc(nx,ny,f,uo,rho);

% Calculate rho, u, v

[rho,u,v]=ruv(nx,ny,f);

count=count+1;

ers=0.;
for i =1:nx
for j=1:ny

ers=ers+u(i,j)*u(i,j)+v(i,j)*v(i,j);

end
end

error=abs(ers-erso);
erso=ers;

end

%Plotting data
result(nx,ny,x,y,u,v,uo,rho);

%Boudary conditions for Channel flow
function [f]=boundary(nx,ny,f,uo,rho)

%right hand boundary

for j=1:ny

f(nx,j,3)=f(nx-1,j,3);
f(nx,j,7)=f(nx-1,j,7);
f(nx,j,6)=f(nx-1,j,6);

end

%bottom, and top boundary, bounce back
for i=1:nx

f(i,1,2)=f(i,1,4);
f(i,1,5)=f(i,1,7);
f(i,1,6)=f(i,1,8);
f(i,ny,4)=f(i,ny,2);
f(i,ny,7)=f(i,ny,5);
f(i,ny,8)=f(i,ny,6);
u(i,1)=0.0; v(i,1)=0.0;
u(i,ny)=0.0; v(i,ny)=0.0;

end
%Left boundary, velocity is given= uo

for j=2:ny-1
f(1,j,1)=f(1,j,3)+2.*rho(1,j)*uo/3.;
f(1,j,5)=f(1,j,7)-0.5*(f(1,j,2)-f(1,j,4))+rho(1,j)*uo/6.;
f(1,j,8)=f(1,j,6)+0.5*(f(1,j,2)-f(1,j,4))+rho(1,j)*uo/6.;
u(1,j)=uo; v(1,j)=0.0;
end

% End of boundary conditions.

end
