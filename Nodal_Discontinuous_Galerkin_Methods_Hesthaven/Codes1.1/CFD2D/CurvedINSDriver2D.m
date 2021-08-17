% Matlab script for computing the solution of the
% incompressible Navier-Stokes equations

Globals2D;

% Choose domain, initial solution and boundary condition functions
%simtype = 'Channel'
simtype = 'Kovasznay'
simtype = 'VolkerCylinder'
%simtype = 'PearsonVortex'
switch(simtype)
  case 'Channel'
    FileName = 'channelA1.neu';
    ExactSolution   = @INSchannelIC2D;
    ExactSolutionBC = @INSchannelBC2D;
    FinalTime= 1; nu = 1/40;
  case 'Kovasznay'
    FileName = 'kovA02.neu';
    ExactSolution   = @KovasznayIC2D;
    ExactSolutionBC = @KovasznayBC2D;
    FinalTime= 1; nu = 1/40;
  case 'VolkerCylinder'
    FileName = 'cylinderA00075b.neu';
%    FileName = 'cylinderCA0015.neu';
    ExactSolution   = @INScylinderIC2D;
    ExactSolutionBC = @INScylinderBC2D;
    FinalTime = 8; nu = 1e-3;
  case 'PearsonVortex'
    FileName = 'pvortex4A01.neu';
    ExactSolution   = @PearsonVortexIC2D;
    ExactSolutionBC = @PearsonVortexBC2D;
    FinalTime = .1; nu = 1e-2;
end

% Order of polynomial approximation (N) 
N = 5;
[Nv, VX, VY, K, EToV, BCType] = MeshReaderGambitBC2D(FileName);

% set up high order grid
StartUp2D; 

curved = []; straight = 1:K;
if(strfind(simtype, 'VolkerCylinder'))
  [k,f] = find(BCType==Cyl);
  cylfaces = [k,f];
  curved = sort(unique(k)); straight = setdiff(1:K, curved);
  MakeCylinder2D(cylfaces, .05, 0, 0);
end

% build boundary condition maps
BuildBCMaps2D;

% evaluate initial data
[Ux, Uy, PR] = feval(ExactSolution, x, y, 0, nu);

% integrate to FinalTime
[Ux, Uy, PR, time] = ...
    CurvedINS2D(Ux, Uy, PR, FinalTime, nu, ...
		simtype, ExactSolution, ExactSolutionBC);
