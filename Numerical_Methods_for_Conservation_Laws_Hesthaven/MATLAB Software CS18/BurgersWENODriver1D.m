% Driver script for solving the 1D Burgers equations using WENO scheme
clear all

% Order of method
m=2;
    
% Set problem parameters
L = 1; FinalTime = 0.2; N = 128; h = L/N; CFL = 0.5;

% Define domain and initial conditions
x = [0:h:L]';
fic = @(x,t) sin(2*pi*x); %periodic BC needed
% fic = @(x,t) (1-sign(x-0.2-3*t))/2+1; % Constant BC needed

% Compute cell averages using Legendre Gauss rule of order NGQ
u = zeros(N+1,1); 
NGQ = 10; [xGQ,wGQ] = LegendreGQ(NGQ);
for i=1:NGQ+1
  u = u + wGQ(i)*fic(x+xGQ(i)*h/2,0);
end;
u = u/2;

% Solve Problem
[u] = BurgersWENO1D(x,u,h,m,CFL,FinalTime);