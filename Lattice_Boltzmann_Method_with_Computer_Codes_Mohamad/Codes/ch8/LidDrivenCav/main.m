%main.m
%LBM- 2-D2Q9, Lid-driven Cavity, Re=1000, note that c2=1/3, w9=4/9,
% w1-4=1/9, and w5-w8, 1/36
clear
nx=101;ny=101;
f=zeros(nx,ny,9);feq=zeros(nx,ny,9);
u=zeros(nx,ny);v=zeros(nx,ny);
rho=ones(nx,ny);x=zeros(nx);y=zeros(ny);
w=[1/9 1/9 1/9 1/9 1/36 1/36 1/36 1/36 4/9];
cx = [1 0 -1  0 1 -1 -1  1 0];
cy = [0 1  0 -1 1  1 -1 -1 0];
c2=1./3.;
dx=1.0;dy=1.0;
xl=1.0; yl=1.0;
dx=xl/(nx-1);
dy=yl/(ny-1);
x=(0:dx:xl);
y=(0:dy:yl);
uo=0.10;
%alpha=0.01;
alpha=0.1;
Re=uo*(ny-1)/alpha
omega=1./(3.*alpha+0.5);
count=0; tol=1.0e-4; error=10.;erso=0.0;
%setting lid velocity

u(:,ny)=uo;

%Main Loop

while error>tol
  disp('----------------')
  disp(error)
  disp(tol)
				% Collitions
  [f]=collision(nx,ny,u,v,cx,cy,omega,f,rho,w);
				% Streaming:

  [f]=stream(f);

				% End of streaming
				%Boundary condition:
  [f]=boundary(nx,ny,f,uo);

				% Calculate rho, u, v

  f(1,1,:)=0; f(1,1,9)=1;
  f(nx,1,:)=0; f(nx,1,9)=1;
  f(1,ny,:)=0; f(1,ny,9)=1;
  f(nx,ny,:)=0; f(nx,ny,9)=1;  

  [rho,u,v]=ruv(nx,ny,f);

  count=count+1;

  ers=0.;
  for i =1:nx
    for j=1:ny
      ers=ers+u(i,j)*u(i,j)+v(i,j)*v(i,j);
    end
  end

  %error=abs(ers-erso);
  error = abs(ers - erso) / (ers + 1e-12);
  erso=ers;

end
				
result(nx,ny,x,y,u,v,uo,rho); %Plotting data
