
%LBM-Sc-model 2-D2Q9, note that c2=1/3, w9=4/9,
% w1-4=1/9, and w5-w8, 1/36
%
%

%
%
%
clear all;
clc;
G=-5.0;
rho_cr=log(2);
rho_liq=1.95;
rho_gas=0.15;
Nsteps=2000;
nx=128;ny=128;
noput=10;
f=zeros(nx,ny,9);feq=zeros(nx,ny,9);
u=zeros(nx,ny);v=zeros(nx,ny);
uf=zeros(nx,ny);vf=zeros(nx,ny);
forcx=zeros(nx,ny);forcy=zeros(nx,ny);
rho=ones(nx,ny);x=zeros(nx);y=zeros(ny);
w=[1/9 1/9 1/9 1/9 1/36 1/36 1/36 1/36 4/9];
cx = [1 0 -1 0 1 -1 -1 1 0];
cy = [0 1 0 -1 1 1 -1 -1 0];
c2=1./3.;
omega=1.;
%setting
for i=1:nx

for j=1:ny
    rho(i,j)=rho_cr+0.1*rand();
end

end
counter_frame=1;
%Main Loop

for counter=1:Nsteps
% force terms

[forcx,forcy]=force(nx,ny,u,v,cx,cy,rho,w,G);

% Collitions
[f]=collision(nx,ny,uf,vf,cx,cy,omega,f,rho,w,forcx,forcy);

% Streaming:

[f]=stream(f);

% End of streaming
%Boundary condition:
[f]=boundary(nx,ny,f);

% Calculate rho, u, v

[rho,u,v]=ruv(nx,ny,f);
for j=1:ny

for i=1:nx

uf(i,j)=u(i,j)+0.5*forcx(i,j)/rho(i,j);
vf(i,j)=v(i,j)+0.5*forcy(i,j)/rho(i,j);
end

end

counter;

if mod(counter,noput)==0

imagesc(rho);
F(counter_frame)=getframe;
counter_frame=counter_frame+1;

end

imagesc(rho)

%
F = getframe(gcf);

%


end

%Plotting data

movie(F,10)

% imagesc(rho);
disp('Rho_liq=')
disp(mean(mean(rho(nx/2-5:nx/2+5,ny/2-5:ny/2+5))))
disp('Rho_gas=')
disp(mean(mean(rho(1:10,1:10))))
