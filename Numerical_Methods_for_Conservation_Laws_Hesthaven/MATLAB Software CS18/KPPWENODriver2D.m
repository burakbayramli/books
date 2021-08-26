% Driver script for solving the 2D KPP equations using a WENO scheme
clear all

% Order of method
m=2;

% Set problem parameters
Nx = 64; Ny = 64; hx = 4.0/Nx; hy = 4.0/Ny; FinalTime = 1.0; CFL = 0.9;

% Define domain and initial conditions
xv = [-2:hx:2]; yv = [-2.5:hy:1.5]; [x,y] = meshgrid(xv,yv);
u = pi/4 + (sqrt(x.^2+y.^2)<=1)*13*pi/4;

% Solve Problem
[u] = KPPWENO2D(x,y,u,hx,hy,m,CFL,FinalTime);