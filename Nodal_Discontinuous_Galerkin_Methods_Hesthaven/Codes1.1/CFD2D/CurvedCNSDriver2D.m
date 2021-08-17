% Driver script for solving the 2D compressible Navier-Stokes equations
Globals2D;

% set up simulation type
%sim = 'CylinderFlow';
%sim = 'BoxFlow';
sim = 'ChannelFlow';

switch sim
case {'CylinderFlow'}
  filename = 'cylinderA00075b.neu';
  InitialSolution = @CylIC2D;
  ExactSolution   = [];
  BCSolution      = @CylBC2D;
case {'ChannelFlow'}
  filename = 'otboxA01.neu';
  InitialSolution = @ChannelIC2D;
  ExactSolution   = @ChannelIC2D;
  BCSolution      = @ChannelBC2D;
case {'BoxFlow'}
  filename = 'otboxA01.neu';
  InitialSolution = @BoxFlowIC2D;
  ExactSolution   = [];
  BCSolution      = @BoxFlowBC2D;
otherwise 
  disp('Simulation case unknown');  stop;
end

% Read in Mesh
[Nv, VX, VY, K, EToV, BCType] = MeshReaderGambitBC2D(filename);

% Set polynomial order
N = 5;

% Initialize solver and construct grid and metric
StartUp2D;

% adjust curved elements if according to simulation type
% hard wired, cylinder (centered at (0,0) with radius 1.0)
[k,f] = find(BCType==Cyl);    curved = []; BCTypeSave = BCType;
if(~isempty(k))
  cylfaces = [k,f];
  curved = sort(unique(k));
  MakeCylinder2D(cylfaces, 1.0, 0, 0);
end

straight = setdiff(1:K, curved);

if(strfind(sim, 'BoxFlow'))
 BuildPeriodicMaps2D(1,1);
end

BuildBCMaps2D;

% compute initial condition (time=0)
Q = zeros(Np, K, 4);
[Q(:,:,1),Q(:,:,2),Q(:,:,3),Q(:,:,4)] = feval(InitialSolution, x, y, 0);

% Solve Problem
FinalTime = .1;
[Q] = CurvedCNS2D(Q, FinalTime, ExactSolution, BCSolution);

