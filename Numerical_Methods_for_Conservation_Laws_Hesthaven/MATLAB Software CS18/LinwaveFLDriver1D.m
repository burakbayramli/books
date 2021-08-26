% Driver script for solving the 1D wave equations using flux-limit scheme
clear all

% Set problem parameters
L = 2; FinalTime = 4.0; N = 512; h = L/N; CFL = 0.9;

% Define domain and initial conditions
x = [-1:h:1]'; 
[u] = wavetest(x,0.5,-0.7,0.005,10,log(2)/(36*0.005^2));
%u = (sign(x+0.5) - sign(x-0.5))/2;

% Solve Problem
[u] = LinwaveFL1D(x,u,h,CFL,FinalTime);