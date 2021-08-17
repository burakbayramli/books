% Driver script for solving the 2D vacuum Euler's equations 
Globals2D;

% Order of polynomial approximation (N) 
N = 1;

fluxtype = 'HLL';
% set up simulation type
%sim = 'IsentropicVortex'; 
%sim = 'Cylinder';
sim = 'ForwardStep'
switch sim
case {'IsentropicVortex'}
  filename = 'vortexA04.neu';
  InitialSolution = @IsentropicVortexIC2D;
  ExactSolution   = @IsentropicVortexIC2D;
  BCSolution      = @IsentropicVortexBC2D;
case {'ForwardStep'} 
  filename = 'fstepA001.neu';
  filename = 'FS_949.neu';
  InitialSolution = @ForwardStepIC2D;
  ExactSolution   = @ForwardStepIC2D;
  BCSolution      = @ForwardStepBC2D;
case 'Cylinder'
  filename        = 'cylinderA00075b.neu';
  InitialSolution = @ForwardStepIC2D;
  ExactSolution   = [];
  BCSolution      = @ForwardStepBC2D;
otherwise 
  disp('Simulation case unknown');  stop;
end

% Read in Mesh
[Nv, VX, VY, K, EToV, BCType] = MeshReaderGambitBC2D(filename);

% Initialize solver and construct grid and metric
StartUp2D;

Q = zeros(Np, K, 1);
%refineflag = ones(K,Nfaces); Q = ConformingHrefine2D(refineflag, Q); 
%refineflag = ones(K,Nfaces); Q = ConformingHrefine2D(refineflag, Q); 

% adjust curved elements according to simulation type
if(strfind(sim, 'Cylinder'))
  [k,f] = find(BCType==Cyl);
  cylfaces = [k,f];
  curved = sort(unique(k)); straight = setdiff(1:K, curved);
  MakeCylinder2D(cylfaces, .05, 0, 0);
  ids = find(BCType(:)==Cyl); BCType(ids) = Wall;
else
  % hard wired, cylinder (centered at (0,0) with radius .5)
  [k,f] = find(BCType==Cyl)
  curved = [];
  if(~isempty(k))
    cylfaces = [k,f];
	curved = sort(unique(k)); 
    MakeCylinder2D(cylfaces, .5, 0, 0);
  end
end
straight = setdiff(1:K, curved);
BuildBCMaps2D;

% compute initial condition (time=0)
Q = feval(InitialSolution, x, y, 0);

% Solve Problem
FinalTime = 4.0;
[Q] = EulerShock2D(Q, FinalTime, ExactSolution, BCSolution, fluxtype); 

