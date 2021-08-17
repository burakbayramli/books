% Driver script for solving the 1D Poisson equation
Globals1D;

% Polynomial order used for approximation 
N =4;

% Read in Mesh
[Nv, VX, K, EToV] = MeshGen1D(0,2*pi,10);

% Initialize solver and construct grid and metric
StartUp1D;

% Set RHS
f = -J.*((invV'*invV)*sin(x));

% Set up operator
[A] = PoissonCstab1D();

% Solve Problem
solvec = A\f(:);
u = reshape(solvec,Np,K);
