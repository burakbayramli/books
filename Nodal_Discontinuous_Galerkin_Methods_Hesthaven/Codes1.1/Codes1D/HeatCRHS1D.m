function [rhsu] = HeatCRHS1D(u,time)

% function [rhsu] = HeatCRHS1D(u,time)
% Purpose  : Evaluate RHS flux in 1D heat equation using central flux

Globals1D;

% Define field differences at faces
du = zeros(Nfp*Nfaces,K); du(:)  = (u(vmapM)-u(vmapP))/2.0;

% impose boundary condition -- Dirichlet BC's
uin  = -u(vmapI); du(mapI) = (u(vmapI)-uin)/2.0; 
uout = -u(vmapO); du(mapO)=(u(vmapO) - uout)/2.0;

% Compute q and form differences at faces
q = rx.*(Dr*u) - LIFT*(Fscale.*(nx.*du));
dq = zeros(Nfp*Nfaces,K); dq(:)  = (q(vmapM)-q(vmapP))/2.0;

% impose boundary condition -- Neumann BC's
qin  = q(vmapI); dq(mapI) = (q(vmapI)- qin )/2.0; 
qout = q(vmapO); dq(mapO) = (q(vmapO)-qout)/2.0;

% compute right hand sides of the semi-discrete PDE
rhsu = rx.*(Dr*q) - LIFT*(Fscale.*(nx.*dq));
return
