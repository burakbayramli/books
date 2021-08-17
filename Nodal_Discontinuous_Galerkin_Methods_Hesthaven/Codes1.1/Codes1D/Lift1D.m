function [LIFT] = Lift1D

% function [LIFT] = Lift1D
% Purpose  : Compute surface integral term in DG formulation

Globals1D;
Emat = zeros(Np,Nfaces*Nfp);

% Define Emat
Emat(1,1) = 1.0; Emat(Np,2) = 1.0;

% inv(mass matrix)*\s_n (L_i,L_j)_{edge_n}
LIFT = V*(V'*Emat);
return
