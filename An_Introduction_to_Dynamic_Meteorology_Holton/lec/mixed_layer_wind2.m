% MATLAB file:   mixed_layer_wind_2.m    (2/13/02)
% Display of mixed layer solution for u and v
% given a specified horizontal geopotential field.
clear all
close all
Lx = 12.e6;  Ly = 6.e6;         % horizontal domain size
cor = 1.e-4;                    % Coriolis parameter
Nx = 22; Ny =22 ;               % number of grid points in each direction
xx=linspace(-Lx/2,Lx/2,Nx);     % Nx gridpoints in x   
yy=linspace( 0,Ly,Ny);          % Ny gridpoints in y
[x,y]=meshgrid(xx,yy);          % Sets matrix for grid system in x and y
%   *********Define the function to be contoured*********
h = 1000;                       % boundary layer depth
k = pi/6.e6;                    % zonal wavenumber in units of 1/m
m=pi/6.e6;                      % meridional wavenumber
U0 = 5;                         % mean zonal velocity
A = 1.5e3;                      % constant value of streamfunction
%
phi =  9800 - (U0*cor)*y +A*sin(k*x).*sin(m*y);   
ug = U0-A/cor*m*sin(k*x).*cos(m*y);
vg = A/cor*k*cos(k*x).*sin(m*y);
kappa = 0.05;                  % coefficient for mixed layer s/m
Km  = 10;                      % Ekman layer diffusion coefficient
%initialize u, v to geostrophic values
u = ug;
v = vg;
Vb = abs(u+i*v);                % initialize wind magnitude to geostrophic
figure (1)
subplot(2,1,1);
cs =contour(x/1000,y/1000,phi);     
axis([-6000 6000 0 6000])
clabel(cs), title('geopotential')
xlabel('x (km)'), ylabel('y (km)');

%  velocity shown in arrows
subplot(2,1,2);
quiver(x/1000,y/1000,ug,vg,'r')
axis([-6000 6000 0  6000])
xlabel('x (km)'), ylabel('y (km)');
title('mixed layer winds')