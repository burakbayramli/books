% Driver script for solving the 1D Euler equations using a spectral Fourier
% collocation method
clear all

% Set problem parameters
FinalTime = 0.2; N = 512; CFL = 1.0; gamma=1.4;
xmin = -1.0; xmax = 1.0;

% Define domain and initial conditions
L = xmax-xmin; x = xmin + L/(2*N+1)*[0:2*N]';

% Define domain, materials and initial conditions
r = zeros(2*N+1,1); ru = zeros(2*N+1,1); E = zeros(2*N+1,1);

% Initialize for Sod's problem
r = (abs(x)>0.5) + (abs(x)<=0.5)*0.125;
E = ((abs(x)>0.5) + (abs(x)<=0.5)*0.1)/(gamma-1);

% Solve Problem
q = zeros(2*N+1,3); q(:,1)=r; q(:,2)=ru; q(:,3)=E;
[q] = EulerSpec1D(x,q,N,L,CFL,gamma,FinalTime);