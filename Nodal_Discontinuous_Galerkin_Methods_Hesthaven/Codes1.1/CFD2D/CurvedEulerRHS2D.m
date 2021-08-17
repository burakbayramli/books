function [rhsQ] = CurvedEulerRHS2D(Q, time, SolutionBC, fluxtype)

% function [rhsQ] = CurvedEulerRHS2D(Q, time, SolutionBC, fluxtype)
% Purpose: compute right hand side residual for the compressible Euler 
%          gas dynamics equations

Globals2D;

% 1.1 Interpolate solution to cubature nodes 
cQ = zeros(cub.Ncub, K, 4);
for n=1:4, cQ(:,:,n) = cub.V*Q(:,:,n); end;

% 1.2 Evaluate flux function at cubature nodes
gamma = 1.4;
[F,G,rho,u,v,p] = EulerFluxes2D(cQ, gamma);

% 1.3 Compute volume terms (dphidx, F) + (dphidy, G)
rhsQ = zeros(Np, K, 4);
for n=1:4
  ddr = (cub.Dr')*(cub.W.*(cub.rx.*F(:,:,n) + cub.ry.*G(:,:,n)));
  dds = (cub.Ds')*(cub.W.*(cub.sx.*F(:,:,n) + cub.sy.*G(:,:,n)));
  rhsQ(:,:,n) = ddr + dds;
end

% 2.1 SURFACE TERMS (using Gauss nodes on element faces)
nx = gauss.nx; ny = gauss.ny; 
mapW = gauss.mapW; mapI = gauss.mapI; mapO = gauss.mapO; mapB = gauss.mapB; mapC = gauss.mapC;

% 2.2 Interpolate solution to Gauss surface nodes
gQM = zeros(gauss.NGauss*Nfaces, K, 4); gQP = zeros(gauss.NGauss*Nfaces, K, 4);
for n=1:4
  gQ = gauss.interp*Q(:,:,n);
  gQM(:,:,n) = gQ(gauss.mapM);  gQP(:,:,n) = gQ(gauss.mapP);
end  

% 2.3 Apply boundary conditions to '+' traces
if(~isempty(SolutionBC))
  gQP = feval(SolutionBC, gauss.x, gauss.y, gauss.nx, gauss.ny, gauss.mapI, ...
                 gauss.mapO, gauss.mapW, gauss.mapC, gQP, time);
end

% 2.4 Evaluate surface flux functions with stabilization
switch fluxtype
  case {'LF'}
    [flux] = EulerLF2D (nx, ny, gQM, gQP, gamma); 
  case {'Roe'}
    [flux] = EulerRoe2D(nx, ny, gQM, gQP, gamma); 
  case {'HLL'}
    [flux] = EulerHLL2D(nx, ny, gQM, gQP, gamma); 
  case {'HLLC'}
    [flux] = EulerHLLC2D(nx, ny, gQM, gQP, gamma); 
end

% 2.5 Compute surface integral terms
for n=1:4
  rhsQ(:,:,n) = rhsQ(:,:,n) - gauss.interp'*(gauss.W.*flux(:,:,n));
end

% 3.1 Multiply by inverse mass matrix
for n=1:4
  % 3.1.a Multiply straight sided elements by inverse mass matrix
  rhsQ(:,straight, n) = V*V'*(rhsQ(:,straight, n)./J(:,straight));

  % 3.1.b Multiply curvilinear elements by custom inverse mass matrices
  Ncurved = length(curved);
  for m=1:Ncurved
    k = curved(m);
    mmCHOL = cub.mmCHOL(:,:,k);
    rhsQ(:,k,n) = mmCHOL\(mmCHOL'\rhsQ(:,k,n));
  end
end
return;
