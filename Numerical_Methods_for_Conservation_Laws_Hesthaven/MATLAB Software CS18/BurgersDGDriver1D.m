% Driver script for solving the 1D Burgers equations using an DG scheme
clear all

% Order of method (m), number of elements (N)
m=1; N=40;
    
% Set problem parameters
xmin = 0.0; xmax = 1.0;
FinalTime = 0.5; CFL = 0.1;

% Generate mesh
VX = (xmax-xmin)*(0:N)/N + xmin; r = LegendreGL(m);
x = ones(m+1,1)*VX(1:N) + 0.5*(r+1)*(VX(2:N+1)-VX(1:N));
h = (xmax-xmin)/N;

% Define initial conditions
u = sin(2*pi*x)+0.8; %periodic BC needed
% u = (1-sign(x-0.2))/2+1; % Constant BC needed

% Solve Problem
[u] = BurgersDG1D(x,u,h,m,N,CFL,FinalTime);