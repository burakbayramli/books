% Driver script for solving the 2D Burgers equations using a WENO scheme
clear all

% Order of method
m=2;
    
% Set problem parameters
Lx = 1; Ly = 1; Nx = 64; Ny = 64; hx = Lx/Nx; hy = Ly/Ny; 
FinalTime = 0.1; CFL = 0.9;

% Define domain and initial conditions
xv = [0:hx:Lx]; yv = [0:hy:Ly]; [x,y] = meshgrid(xv,yv);
u = sin(4*pi*(x+y/2));

% Solve Problem
[u] = BurgersWENO2D(x,y,u,hx,hy,m,CFL,FinalTime);