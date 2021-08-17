% Driver script for solving the 2D vacuum Maxwell's equations on TM form
Globals2D;

% Polynomial order used for approximation 
N = 10;

% Read in Mesh
[Nv, VX, VY, K, EToV] = MeshReaderGambit2D('Maxwell025.neu');

% Initialize solver and construct grid and metric
StartUp2D;

% Set initial conditions
mmode = 1; nmode = 1;
Ez = sin(mmode*pi*x).*sin(nmode*pi*y); Hx = zeros(Np, K); Hy = zeros(Np, K);

% Solve Problem
FinalTime = 1;
[Hx,Hy,Ez,time] = Maxwell2D(Hx,Hy,Ez,FinalTime);
