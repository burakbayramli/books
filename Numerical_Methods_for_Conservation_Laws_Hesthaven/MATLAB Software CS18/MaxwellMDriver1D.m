% Driver script for solving the 1D Maxwells equations using a monotone scheme
clear all

% Set problem parameters
L = 2; FinalTime = pi/2; N = 1024; h = L/N; CFL = 0.90;
epl = 1.0; mul = 1.0; epr = 2.25; mur = 1.0;

% Define domain, materials and initial conditions
x = [-1:h:1]';
[Ef Hf ep mu] = CavityExact(x, epl, epr, mul, mur, 0); 

% Solve Problem
EM = [Ef Hf];
[EM] = MaxwellM1D(x,EM,ep,mu,h,CFL,FinalTime);