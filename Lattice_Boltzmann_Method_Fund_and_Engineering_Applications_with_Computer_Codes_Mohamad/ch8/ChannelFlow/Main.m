%Main.m
%LBM- 2-D2Q9, channel, Re=400, note that c2=1/3, w9=4/9,
% w1-4=1/9, and w5-w8, 1/36
clear
nx=1001;ny=41;
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
uo=0.2;
alpha=0.02;
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
