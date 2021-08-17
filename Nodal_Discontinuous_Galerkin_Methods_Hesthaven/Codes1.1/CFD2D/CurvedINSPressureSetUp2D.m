function [PRperm, PRsystemC, PRsystemCT, rhsbcPR] = CurvedINSPressureSetUp2D(dt, nu, BCfunction)

% function [PRperm, PRsystemC, PRsystemCT, rhsbcPR] = CurvedINSPressureSetUp2D(dt, nu, BCfunction)
% Purpose: build pressure system and boundary forcing term

Globals2D;

% choose order to integrate exactly
Nint = ceil(3*N/2);

% build cubature nodes for all elements
CubatureOrder = 2*(Nint+1); cub = CubatureVolumeMesh2D(CubatureOrder);

% build Gauss node data for all element faces
NGauss = (Nint+1); gauss = GaussFaceMesh2D(NGauss);

% Convert boundary conditions to Dirichlet & Neumann
saveBCType = BCType;
ids = find(saveBCType==Out); BCType(ids) = Dirichlet;
ids = find(saveBCType==In | saveBCType==Wall | saveBCType==Cyl);  
BCType(ids) = Neuman;

% Form inhomogeneous boundary term for rhs data 
[bcUx,bcUy,bcPR] = ...
feval(BCfunction, gauss.x, gauss.y, gauss.nx, gauss.ny,...
      gauss.mapI, gauss.mapO, gauss.mapW, gauss.mapC, 0, nu);  

% Build pressure boundary condition forcing vector
[PRsystemBC] = CurvedPoissonIPDGbc2D();
rhsbcPR = PRsystemBC*bcPR(:);

% Build pressure system (all Neumann, excluding outflow)
[PRsystem, mm] = CurvedPoissonIPDG2D();

% Restore original boundary types
BCType = saveBCType;

% Find Reverse-Cuthill-Mckee ordering to reduce bandwith of Pressure matrix
PRperm     = symrcm(PRsystem);

% Apply row and column permutation to Pressure matrix
PRsystem   = PRsystem(PRperm, PRperm);

% Compute Cholesky factorization of Pressure matrix
PRsystemC  = chol(PRsystem); PRsystemCT = transpose(PRsystemC);
return;
