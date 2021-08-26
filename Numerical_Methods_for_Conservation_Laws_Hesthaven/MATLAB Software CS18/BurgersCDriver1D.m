% Driver script for solving the 1D Burgers equations using second order central scheme
clear all

% Set problem parameters
L = 1; FinalTime = 0.2; N = 512; h = L/N; CFL = 0.9;

% Define domain and initial conditions
x = [0:h:1]'; 
u = sin(2*pi*x); %periodic BC needed
% u = (1-sign(x-0.2))/2+1; % Constant BC needed

% Solve Problem
[u] = BurgersC1D(x,u,h,CFL,FinalTime);