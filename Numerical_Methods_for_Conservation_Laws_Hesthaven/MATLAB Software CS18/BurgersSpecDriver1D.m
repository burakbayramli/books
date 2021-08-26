% Driver script for solving the 1D Burgers equations using a Fourier
% spectral collocation method
clear all

% Set problem parameters
FinalTime = 0.3; N = 256; CFL = 1.0;
xmin = 0.0; xmax = 1.0;

% Define domain and initial conditions
L = xmax-xmin;
x = xmin + L/(2*N+1)*[0:2*N]';
fic = @(x,t) sin(2*pi*x); 

% Compute initial conditions
u = fic(x,0);

% Solve Problem
[u] = BurgersSpec1D(x,u,N,L,CFL,FinalTime);