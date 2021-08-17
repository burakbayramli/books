function [PRperm, PRmask, PRsystemC, PRsystemCT, PRsystemBC, VELperm, ...
           VELsystemC, VELsystemCT, VELsystemBC, PRsystem, VELsystem] = ...
              CurvedINSMatrixSystems2D(dt, nu, g0)

% function [PRperm, PRsystemC, PRsystemCT, PRsystemBC, VELperm, VELsystemC, VELsystemCT, VELsystemBC] = ...
%      CurvedINSMatrixSystems2D(dt, nu, g0)
% Purpose: build velocity and pressure matrices for dual time stepping scheme

Globals2D;

% Build pressure system (all Neumann, excluding outflow)
saveBCType = BCType;
if(1)
  ids = find(saveBCType==Out);                    BCType(ids) = Dirichlet;
  ids = find(saveBCType==In | saveBCType==Wall);  BCType(ids) = Neuman;
else
  ids = find(saveBCType); BCType(ids) = Dirichlet;
end

[PRsystem, mm] = CurvedPoissonIPDG2D();
[PRsystemBC] = CurvedPoissonIPDGbc2D();

PRmask = ones(Np*K,1);

BCType = saveBCType;

PRperm     = symrcm(PRsystem);
PRsystem   = PRsystem(PRperm, PRperm);

PRsystemC  = chol(PRsystem);
PRsystemCT = transpose(PRsystemC);

% Build velocity system (all Dirichlet, excluding outflow)
if(1)
  ids = find(saveBCType==In | saveBCType==Wall);  BCType(ids) = Dirichlet;
  ids = find(saveBCType==Out);                    BCType(ids) = Neuman;
  else
  % THIS WORKED FOR KOV
  ids = find(saveBCType); BCType(ids) = Dirichlet;
end

[VELsystem, mm] = CurvedPoissonIPDG2D();
[VELsystemBC] = CurvedPoissonIPDGbc2D();

VELsystem = g0*mm/(dt*nu) + VELsystem;

% bandwidth minimize the velocity matrix with reverse Cuthill-McKee
VELperm    = symrcm(VELsystem);
VELsystem  = VELsystem(VELperm,VELperm);
VELsystemC = chol(VELsystem);
VELsystemCT = transpose(VELsystemC);

BCType = saveBCType;
return
