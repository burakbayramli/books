% Driver script for solving the 2D Euler equations
Globals2D;

% Order of polynomials used for approximation 
N = 9;

% Read in Mesh
sim = 'IsentropicVortex';
switch sim
case {'IsentropicVortex'}
  filename = 'vortexA04.neu';
  InitialSolution = @IsentropicVortexIC2D;
  ExactSolution   = @IsentropicVortexIC2D;
  BCSolution      = @IsentropicVortexBC2D;
case {'ChannelFlow'}
  filename = 'Euler01.neu';
  InitialSolution = @ChannelIC2D;
  ExactSolution   = [];
  BCSolution      = @ChannelBC2D;
otherwise 
  disp('Simulation case unknown');  stop;
end

% read mesh from file
[Nv, VX, VY, K, EToV, BCType] = MeshReaderGambitBC2D(filename);

% set up nodes and basic operations
StartUp2D;

% turn cylinders into walls
ids = find(BCType==Cyl); 
BCType(ids) = Wall;

BuildBCMaps2D

% compute initial condition
Q = feval(InitialSolution, x, y, 0);

% Solve Problem
FinalTime = 1;
[Q] = Euler2D(Q, FinalTime, BCSolution); 

