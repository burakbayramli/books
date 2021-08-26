% Driver script for solving the 2D Burgers equations using flux-limited scheme
clear all

% Set problem parameters
Lx = 1; Ly = 1; Nx = 128; Ny = 128; hx = Lx/Nx; hy = Ly/Ny; 
FinalTime = 0.1; CFL = 0.9;

% Define domain and initial conditions
xv = [0:hx:Lx]; yv = [0:hy:Ly]; [x,y] = meshgrid(xv,yv);
u = sin(4*pi*(x+y/2));

% Solve Problem
[u] = BurgersFL2D(x,y,u,hx,hy,CFL,FinalTime);