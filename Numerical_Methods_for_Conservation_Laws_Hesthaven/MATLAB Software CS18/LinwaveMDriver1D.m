% Driver script for solving the 1D wave equation using a monotone scheme
clear all

% Set problem parameters
L = 2; FinalTime = 4.0; N = 2048; h = L/N; CFL = 0.90;

% Define domain and initial conditions
x = [-1:h:1]'; 
[u] = wavetest(x,0.5,-0.7,0.005,10,log(2)/(36*0.005^2));

% Solve Problem
[u] = LinwaveM1D(x,u,h,CFL,FinalTime);