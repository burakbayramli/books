%LBM- 2-D2Q9, Differentially Heated, note that c2=1/3, w9=4/9,
% w1-4=1/9, and w5-w8, 1/36
clear
nx=81;ny=81;
f=zeros(nx,ny,9);g=zeros(nx,ny,9);rhog=ones(nx,ny);
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
x=(0:dx:xl);
y=(0:dy:yl);
alpha=0.05;
omega=1./(3.*alpha+0.5);
pr=0.71;
Ra=1.0e5;
alphag=alpha/pr;
ym1=real(ny-1);
gbeta=Ra*alpha*alphag/(ym1*ym1*ym1);
omegag=1.0/(3.*alphag+0.5);
count=0;
%Main Loop

while count< 70000

% Collitions
[f]=collition(nx,ny,u,v,cx,cy,omega,f,rho,w,rhog,gbeta);
% Streaming:

[f]=stream(f);

% End of streaming
%Boundary condition:
[f]=boundary(nx,ny,f);

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

end
%Plotting data
result(nx,ny,x,y,u,v,rho,rhog);
