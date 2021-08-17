function [rhsu] = HeatCstabRHS1D(u,time)

% function [rhsu] = HeatCstabRHS1D(u,time)
% Purpose  : Evaluate RHS flux in 1D heat equation using stabilized central flux

Globals1D;

% Define field differences at faces
du   = zeros(Nfp*Nfaces,K); du(:) = (u(vmapM)-u(vmapP))/2.0;

% impose boundary condition -- Dirichlet BC's
uin  = -u(vmapI); du (mapI) = (u(vmapI) -  uin )/2.0;
uout = -u(vmapO); du (mapO) = (u(vmapO) - uout)/2.0;

% Compute q
q = rx.*(Dr*u) - LIFT*(Fscale.*(nx.*du));
dq = zeros(Nfp*Nfaces,K);dq(:) = q(vmapM)-q(vmapP);

% impose boundary condition -- Neumann BC's
qin = q(vmapI);  dq (mapI) = q(vmapI)-  qin; 
qout = q(vmapO); dq (mapO) = q(vmapO)- qout;

% evaluate fluxes
tau = 1.0;
fluxq = nx.*(dq/2.0+tau*nx.*du);

% compute right hand sides of the semi-discrete PDE
rhsu = rx.*(Dr*q) - LIFT*(Fscale.*fluxq);
return
