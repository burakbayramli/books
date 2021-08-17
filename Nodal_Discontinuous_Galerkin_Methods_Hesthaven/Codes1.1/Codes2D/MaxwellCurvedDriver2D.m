% Test script for solving the 2D advection
Globals2D;

% Set polynomial order to use
N = 7;

% Read and initiate circular mesh
filename = 'circA01.neu';
[Nv, VX, VY, K, EToV, BCType] = MeshReaderGambitBC2D(filename);

StartUp2D;
BuildBCMaps2D;

% Push all boundary faces to unit cylinder
[k,f] = find(BCType);  
curved = sort(unique(k));
MakeCylinder2D([k,f], 1, 0, 0);

% Set initial conditions
% First 6 modes of eigenmodes with 6 azimuthal periods
alpha = [9.936109524217684,13.589290170541217,17.003819667816014,...
         20.320789213566506,23.586084435581391,26.820151983411403];

% choose radial mode
alpha0 = alpha(2);
theta = atan2(y,x);
rad   = sqrt(x.^2+y.^2);

Ez = besselj(6, alpha0*rad).*cos(6*theta);
Hx = zeros(Np, K); Hy = zeros(Np, K);

% Solve Problem for exactly one period
FinalTime = .5;
[Hx,Hy,Ez,time] = MaxwellCurved2D(Hx,Hy,Ez,FinalTime);

exactEz = besselj(6, alpha0*rad).*cos(6*theta)*cos(alpha0*time(end));
maxabserror = max(max(abs(Ez-exactEz)))
