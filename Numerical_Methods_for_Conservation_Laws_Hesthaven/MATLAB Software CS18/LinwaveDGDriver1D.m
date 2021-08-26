% Driver script for solving the 1D wave equations using an DG scheme
clear all

% Order of method (m), number of elements (N)
m=1; N=128;

% Set problem parameters
FinalTime = 4.0; CFL = 0.20;
xmin = -1; xmax = 1;

% Generate mesh
VX = (xmax-xmin)*(0:N)/N + xmin; r = LegendreGL(m);
x = ones(m+1,1)*VX(1:N) + 0.5*(r+1)*(VX(2:N+1)-VX(1:N));
h = (xmax-xmin)/N;

% Define initial conditions
uh = wavetest(x(:),0.5,-0.7,0.005,10,log(2)/(36*0.005^2));
u = reshape(uh,m+1,N);

% Solve Problem
[u] = LinwaveDG1D(x,u,h,m,N,CFL,FinalTime);