% Driver script for solving the 2D Poisson equation on curvilinear domains
Globals2D;

% Polynomial order used for approximation 
N = 6;

% Read in Mesh
[Nv, VX, VY, K, EToV] = MeshReaderGambitBC2D('circA01.neu');

% Initialize solver and construct grid and metric
StartUp2D;

% hard coded for all Dirichlet boundaries to be on cylinder
[k,f] = find(BCType==Dirichlet);
if(~isempty(k))
  cylfaces = [k,f];
  curved = sort(unique(k)); straight = setdiff(1:K, curved);
  MakeCylinder2D(cylfaces, 1, 0, 0);
end

% choose order to integrate exactly
Nint = ceil(2*N/2);

% build cubature nodes for all elements
CubatureOrder = 2*(Nint+1); cub = CubatureVolumeMesh2D(CubatureOrder);
  
% build Gauss node data for all element faces
NGauss = (Nint+1); gauss = GaussFaceMesh2D(NGauss);
  
% build weak Poisson operator matrices
[A, M] = CurvedPoissonIPDG2D();
Abc = CurvedPoissonIPDGbc2D();

% set up right hand side
f = (-2*pi^2-1)*sin(pi*x).*sin(pi*y); 

% set up boundary condition 
ubc = zeros(gauss.NGauss*Nfaces*K,1);
xbc = gauss.x(gauss.mapD); ybc = gauss.y(gauss.mapD);
ubc(gauss.mapD) = sin(pi*xbc).*sin(pi*ybc);
xbc = gauss.x(gauss.mapN); ybc = gauss.y(gauss.mapN);
ubc(gauss.mapN) = ...
    gauss.nx(gauss.mapN).*(pi*cos(pi*xbc).*sin(pi*ybc)) + ...
    gauss.ny(gauss.mapN).*(pi*sin(pi*xbc).*cos(pi*ybc));

ubc = Abc*ubc;

% solve linear system
solvec = (A+M)\(M*(-f(:)) + ubc);
u = reshape(solvec, Np, K);
