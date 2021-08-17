% Driver script for solving the 3D vacuum Maxwell's equations

Globals3D;

% Polynomial order of approximation 
N = 6;

% Read in Mesh
[Nv, VX, VY, VZ, K, EToV] = MeshReaderGambit3D('cubeK6.neu');

% Initialize solver and construct grid and metric
StartUp3D;

% Set initial conditions
mmode = 1; nmode = 1;

% Use TM mode Maxwell's initial condition 
Hx = zeros(Np, K); Hy = zeros(Np, K); Hz = zeros(Np, K);
Ex = zeros(Np, K); Ey = zeros(Np, K); 

%Ez = exp(-20*(x.^2 + y.^2));
xmode = 1; ymode = 1; 
Ez = sin(xmode*pi*x).*sin(ymode*pi*y);

% Solve Problem
FinalTime = 10;

[Hx,Hy,Hz,Ex,Ey,Ez] = Maxwell3D(Hx,Hy,Hz,Ex,Ey,Ez,FinalTime);

