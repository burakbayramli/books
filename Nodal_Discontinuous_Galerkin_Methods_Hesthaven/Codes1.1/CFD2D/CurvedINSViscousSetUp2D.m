function [VELperm, VELsystemC, VELsystemCT, rhsbcUx, rhsbcUy] = ...
      CurvedINSViscousSetUp2D(dt, nu, g0, BCfunction)

% function [VELperm, VELsystemC, VELsystemCT, rhsbcUx, rhsbcUy] = ...
%      CurvedINSViscousSetUp2D(dt, nu, g0, BCfunction)
% Purpose: build velocity system and boundary forcing terms

Globals2D;

% choose order to integrate exactly
Nint = ceil(3*N/2);

% build cubature nodes for all elements
CubatureOrder = 2*(Nint+1); cub = CubatureVolumeMesh2D(CubatureOrder);

% build Gauss node data for all element faces
NGauss = (Nint+1); gauss = GaussFaceMesh2D(NGauss);

% Convert boundary conditions to Dirichlet & Neumann
saveBCType = BCType;
ids = find(saveBCType==In | saveBCType==Wall | saveBCType==Cyl);  BCType(ids) = Dirichlet;
ids = find(saveBCType==Out);                    BCType(ids) = Neuman;

% Form inhomogeneous boundary term for rhs data (assumes time independent forcing)
[bcUx,bcUy,bcPR] = feval(BCfunction, gauss.x, gauss.y, gauss.nx, gauss.ny, gauss.mapI, gauss.mapO, gauss.mapW, gauss.mapC, 0, nu);  

% Build pressure boundary condition forcing vector
[VELsystemBC] = CurvedPoissonIPDGbc2D();
rhsbcUx = VELsystemBC*bcUx(:); rhsbcUy = VELsystemBC*bcUy(:);

% Build velocity system 
[VELsystem, mm] = CurvedPoissonIPDG2D();
VELsystem = g0*mm/(dt*nu) + VELsystem;

% Restore original boundary types
BCType = saveBCType;

% Find Reverse-Cuthill-Mckee ordering to reduce bandwith of velocity system
VELperm    = symrcm(VELsystem);

% Apply row and column permutation to velocity matrix
VELsystem  = VELsystem(VELperm,VELperm);

% Compute Cholesky factorization of velocity matrix
VELsystemC  = chol(VELsystem); VELsystemCT = transpose(VELsystemC);
return
