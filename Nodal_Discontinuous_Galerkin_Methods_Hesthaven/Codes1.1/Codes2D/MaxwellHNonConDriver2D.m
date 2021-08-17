% Test script for solving the 2D advection
Globals2D;

% Set polynomial order
N = 7;

% Read and set up simple mesh
filename = 'Maxwell05.neu'
[Nv, VX, VY, K, EToV, BCType] = MeshReaderGambitBC2D(filename);

StartUp2D;

% make boundary conditions all Wall type
BCType = Wall*(EToE==((1:K)'*ones(1,Nfaces)));

% create a non-conforming interface by refinement
refineflag = zeros(K,1);
refineflag(1:5) = 1;
Hrefine2D(refineflag);
StartUp2D;
BuildBCMaps2D;

refineflag = zeros(K,1);
refineflag(end) = 1;
Hrefine2D(refineflag);
StartUp2D;
BuildBCMaps2D;

% Set initial conditions
mmode = 1; nmode = 1;
Ez = sin(mmode*pi*x).*sin(nmode*pi*y);
Hx = zeros(Np, K); Hy = zeros(Np, K);

% Solve Problem for exactly one period
FinalTime = sqrt(2);
[Hx,Hy,Ez,time] = MaxwellHNonCon2D(Hx,Hy,Ez,FinalTime);

exactEz = sin(mmode*pi*x).*sin(nmode*pi*y);
maxabserror = max(max(abs(Ez-exactEz)))
     
