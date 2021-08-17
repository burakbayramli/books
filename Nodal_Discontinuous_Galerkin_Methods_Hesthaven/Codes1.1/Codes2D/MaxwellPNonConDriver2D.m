% Test script for solving the 2D advection
Globals2D;

% Generate simple mesh
filename = 'Maxwell025.neu';

[Nv, VX, VY, K, EToV] = MeshReaderGambit2D(filename);
Nfaces = 3;
[EToE,EToF] = tiConnect2D(EToV);
BCType = Wall*(EToE==((1:K)'*ones(1,Nfaces)));

% Build mesh 
Norder = ceil(10*rand(K,1));

% Set up arbitrary order elements mesh
[pinfo] = BuildPNonCon2D(Norder, K, VX, VY, EToV, BCType);

% Set initial conditions
mmode = 1; nmode = 1;

x = []; y = [];
for N1=1:max(Norder)
  pinf = pinfo(N1);
  x(pinf.ids) = pinf.x;
  y(pinf.ids) = pinf.y;
end
x = x'; y = y';
Ez = sin(mmode*pi*x).*sin(nmode*pi*y);
Hx = zeros(size(x)); Hy = zeros(size(x));

% Solve Problem for exactly one period
FinalTime = 1;
[Hx,Hy,Ez,time] = MaxwellPNonCon2D(pinfo, Hx,Hy,Ez,FinalTime);

