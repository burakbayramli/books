% Driver script for solving the 1D Maxwells equations using a spectral 
% Fourier collocation scheme
clear all

% Set problem parameters
FinalTime = pi/2; N = 64; CFL = 0.1;
xmin = -2.0; xmax = 2.0;

% Define domain and initial conditions
L = xmax-xmin; x = xmin + L/(2*N+1)*[0:2*N]';
epl = 1.0; mul = 1.0; epr = 2.25; mur = 1.0;

[Ef, Hf, ep, mu] = CavityExactLong(x, epl, epr, mul, mur, 0);

% Solve Problem
q = [Ef Hf];
[q] = MaxwellSpec1D(x,q,ep,mu,N,L,CFL,FinalTime);