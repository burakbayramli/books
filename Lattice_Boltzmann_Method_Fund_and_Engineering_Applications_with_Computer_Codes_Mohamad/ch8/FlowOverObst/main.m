%main.m

%LBM- 2-D2Q9, flow over an obstacle, Re=400, note that c2=1/3, w9=4/9,
% w1-4=1/9, and w5-w8, 1/36
clear
nx=501;ny=81;
uo=0.1;
f=zeros(nx,ny,9);feq=zeros(nx,ny,9);utim=zeros(1001);count=zeros(1001);
u=uo*ones(nx,ny);v=zeros(nx,ny);
rho=2.*ones(nx,ny);x=zeros(nx);y=zeros(ny);
Tm=zeros(nx);w(9)=zeros; Tvm=zeros(nx);
w=[1/9 1/9 1/9 1/9 1/36 1/36 1/36 1/36 4/9];
cx = [1 0 -1
cy = [0 1 0 -1 1
c2=1./3.;
dx=1.0;dy=1.0;
xl=(nx-1)/(ny-1); yl=1.0;
x=(0:1:nx-1);
y=(0:1:ny-1);
alpha=0.01;
ReH=uo*(ny-1)/alpha
ReD=uo*10./alpha
omega=1./(3.*alpha+0.5);
count(1)=0;

%setting velocity

for j=2:ny-1

u(1,j)=uo;

end

%Main Loop

for kk=1:8000
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

count(kk)=kk;
utim(kk)=rho((nx-1)/2,(ny-1)/2);

end

%Plotting data
result(nx,ny,x,y,u,v,uo,rho,count,utim);
