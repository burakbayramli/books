% Driver script for solving the 3D Poisson equation
Globals3D;

% Generate Mesh
figure(1);
%h = .33;
% [Nv, VX, VY, VZ, K, EToV] = MeshGenDistMesh3D(h);
[Nv, VX, VY, VZ, K, EToV] = MeshReaderGambit3D('cubeK86.neu');

% polynomial order to use in each element
N = 4;
    
% Initialize solver and construct grid and metric
StartUp3D;

% build 3D IPDG Poisson matrix (assuming all Dirichlet)
[A, M] = PoissonIPDG3D();

% perform symmetric reverse Cuthill McKee on stiffness matrix
P = symrcm(A); 
A = A(P,P);
C = cholinc(A, 1e-4);

% set up boundary condition 
xbc = Fx(mapB); ybc = Fy(mapB); zbc = Fz(mapB);
ubc = zeros(Nfp*Nfaces*K,1);
ubc(mapB) = sin(pi*xbc).*sin(pi*ybc).*sin(pi*zbc);

% form right hand side contribution from boundary condition
Abc = PoissonIPDGbc3D(ubc);

% evaluate forcing function
f = -3*(pi^2)*sin(pi*x).*sin(pi*y).*sin(pi*z); 

% set up right hand side for variational Poisson equation
rhs = M*(-f(:)) + Abc(:);

rhs = rhs(P);

% solve use preconditioned conjugate gradient
u = pcg(A, rhs(:), 1e-9, 10000, C', C);

% Shuffle ordering
u(P) = u;
u = reshape(u, Np, K);
