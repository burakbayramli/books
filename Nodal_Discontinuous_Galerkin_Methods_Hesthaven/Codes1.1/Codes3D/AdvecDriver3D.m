% Driver script for solving the 3D advection equations
Globals3D;

% Order of polymomials used for approximation 
N = 8;

% Generate simple mesh
filename = 'cubeK6.neu'
[Nv, VX, VY, VZ, K, EToV] = MeshReaderGambit3D(filename);

% Initialize solver and construct grid and metric
StartUp3D;

% set initial conditions
u = exp(-1*(x.^2 + y.^2 + z.^2));

% Solve Problem
FinalTime = 0.8;
[u] = Advec3D(u,FinalTime);
