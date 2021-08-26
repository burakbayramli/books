% Driver script for solving the 2D Euler equations using second order central scheme
clear all

% Set general problem parameters 
CFL = 0.5; gamma= 1.4;

% Define domain and initial conditions for isentropic vortex
% FinalTime = 0.1;
% Lx = 10; Ly = 10; Nx = 512; Ny = 512; hx = Lx/Nx; hy = Ly/Ny; 
% xv = [-5:hx:5]; yv = [-5:hy:5]; [x,y] = meshgrid(xv,yv);
% x0=0.0; y0=0.0; u0=1.0; v0=0; beta=5.0;
% [r,ru,rv,E] = IsentropicVortex2D(x,x0,u0,y,y0,v0,gamma,beta,0);

% Set problem parameters for 2D Riemann problems
Lx = 1; Ly = 1; Nx = 199; Ny = 199; hx = Lx/Nx; hy = Ly/Ny; 
RiemannProbCase = 4;

% Define domain and initial conditions
xv = [0:hx:Lx]; yv = [0:hy:Ly]; [x,y] = meshgrid(xv,yv);
[r,ru,rv,E,FinalTime] = Riemann2D(x,y,gamma,RiemannProbCase);

% Solve Problem
q = zeros(Ny+1,Nx+1,4);
q(:,:,1) = r; q(:,:,2)=ru; q(:,:,3)=rv; q(:,:,4)=E; 
[q] = EulerC2D(x,y,q,hx,hy,gamma,CFL,FinalTime);