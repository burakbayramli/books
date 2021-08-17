% Driver script for solving the 1D Burgers equations
Globals1D;

% Order of polymomials used for approximation 
N = 4;

% Generate simple mesh
xL = -1.0; xR = 1.0;
[Nv, VX, K, EToV] = MeshGen1D(xL,xR,8);

% Initialize solver and construct grid and metric
StartUp1D;

% Set initial conditions
epsilon = 0.1;
u = -tanh((x+0.5)/(2*epsilon)) + 1.0;

% Solve Problem
FinalTime = 1.5;
[u] = Burgers1D(u,epsilon,xL,xR,FinalTime);
