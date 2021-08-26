% Driver script for solving the 1D Burgers equation using a monotone scheme
clear all

% Set problem parameters
L = 2; FinalTime = 0.5; N = 1024; h = L/N; CFL = 0.90;

% Define domain and initial conditions
x = [0:h:1]'; 

u = sin(2*pi*x); %periodic BC needed
% u = (1-sign(x-0.2))/2+1; % Constant BC needed

% Solve Problem
[u] = BurgersM1D(x,u,h,CFL,FinalTime);