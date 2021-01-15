%LBM- 2-D2Q9, Heated Lid-driven Cavity, Re=1000, note that
c2=1/3, w9=4/9,
% w1-4=1/9, and w5-w8, 1/36
clear
nx=801;ny=41;
f=zeros(nx,ny,9);g=zeros(nx,ny,9);rhog=zeros(nx,ny);
u=zeros(nx,ny);v=zeros(nx,ny);
rho=ones(nx,ny);x=zeros(nx);y=zeros(ny);
w=[1/9 1/9 1/9 1/9 1/36 1/36 1/36 1/36 4/9];
cx = [1 0 -1 0 1 -1 -1 1 0];
cy = [0 1 0 -1 1 1 -1 -1 0];
c2=1./3.;
dx=1.0;dy=1.0;
xl=1.0; yl=1.0;
dx=xl/(nx-1);
dy=yl/(ny-1);
x=(0:1:nx-1);
y=(0:1:ny-1);
uo=0.10;
alpha=0.04;
Re=uo*(ny-1)/alpha
omega=1./(3.*alpha+0.5);
pr=3.8;
alphag=alpha/pr;
omegag=1.0/(3.*alphag+0.5);
count=0; tol=1.0e-5; error=10.;erso=0.0;
%setting lid velocity to uo

u(:,ny)=uo;

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
%Temperature

[g]=gcol(nx,ny,u,v,cx,cy,omegag,g,rhog,w);
[g]=stream(g);
[g]=gbound(nx,ny,w,g);

%streaming

% boundary conditions

%collision

rhog=sum(g,3);
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
result(nx,ny,x,y,u,v,uo,rho,rhog);
