% Driver script for solving the 2D Poisson equation
Globals2D;

% Polynomial order used for approximation 
N = 5;

% Read in Mesh
[Nv, VX, VY, K, EToV] = MeshReaderGambitBC2D('circA01.neu');

% Initialize solver and construct grid and metric
StartUp2D;

% set up boundary conditions
BuildBCMaps2D;

% set up right hand side for homogeneous Poisson 
%[A,M] = Poisson2D(); % Setup using PoissonRHS2D.m
[A,M] = PoissonIPDG2D(); % Setup using PoissonIPDG2D.m

% set up Dirichlet boundary conditions
uD = zeros(Nfp*Nfaces, K);
uD(mapD) = sin(pi*Fx(mapD)).*sin(pi*Fy(mapD));

% set up Neumann boundary conditions
qN = zeros(Nfp*Nfaces, K);
qN(mapN) = nx(mapN).*(pi*cos(pi*Fx(mapN)).*sin(pi*Fy(mapN))) + ...
           ny(mapN).*(pi*sin(pi*Fx(mapN)).*cos(pi*Fy(mapN))) ;

% evaluate boundary condition contribution to rhs
Aqbc = PoissonIPDGbc2D(uD, qN);

% set up right hand side forcing
rhs = -2*(pi^2)*sin(pi*x).*sin(pi*y);
rhs = -MassMatrix*(J.*rhs) + Aqbc;

% solve system
u = A\rhs(:);
u = reshape(u, Np, K);
