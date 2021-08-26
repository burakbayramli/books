% Driver script for solving the 1D Maxwells equations using DG scheme
clear all

% Order of method (m), number of elements (N)
m=4; N=10;

% Set problem parameters
xmin = -1.0; xmax = 1.0;
FinalTime = sqrt(2.0); CFL = 0.25;
epl = 1.0; mul = 1.0; epr = 2.0; mur = 1.0;

% Generate mesh
VX = (xmax-xmin)*(0:N)/N + xmin; r = LegendreGL(m);
x = ones(m+1,1)*VX(1:N) + 0.5*(r+1)*(VX(2:N+1)-VX(1:N));
h = (xmax-xmin)/N;

% Define domain, materials and initial conditions
Ef = zeros(m+1,N); Hf = zeros(m+1,N);
for k = 1:N
   [Ef(:,k), Hf(:,k), ep(:,k), mu(:,k)] = ...
       CavityExact(x(:,k), epl, epr, mul, mur, 0);
end

% Set up material parameters
eps1 = [epl*ones(1,N/2), epr*ones(1,N/2)]; 
mu1 = [mul*ones(1,N/2), mur*ones(1,N/2)]; 
ep = ones(m+1,1)*eps1; mu = ones(m+1,1)*mu1;

% Solve Problem
q = [Ef Hf];
[q] = MaxwellDG1D(x,q,ep,mu,h,m,N,CFL,FinalTime);