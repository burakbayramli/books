function [rhsu] = HeatLDGRHS1D(u,time)

% function [rhsu] = HeatLDGRHS1D(u,time,a,ax)
% Purpose  : Evaluate RHS flux in 1D heat equation using an LDG flux

Globals1D;

% Define field differences at faces
du = zeros(Nfp*Nfaces,K); du(:) = (1.0+nx(:)).*(u(vmapM)-u(vmapP))/2.0;

% impose boundary condition -- Dirichlet BC's
uin  = -u(vmapI); du(mapI) = (1.0+nx(mapI)).*(u(vmapI)- uin)/2.0;
uout = -u(vmapO); du(mapO) = (1.0+nx(mapO)).*(u(vmapO)-uout)/2.0;

% Compute q
q = rx.*(Dr*u)- LIFT*(Fscale.*(nx.*du));
dq = zeros(Nfp*Nfaces,K); dq(:) = (1.0-nx(:)).*(q(vmapM)-q(vmapP))/2.0;

% impose boundary condition -- Neumann BC's
qin  = q(vmapI); dq(mapI) = (1.0-nx(mapI)).*(q(vmapI)- qin)/2.0;
qout = q(vmapO); dq(mapO) = (1.0-nx(mapO)).*(q(vmapO)-qout)/2.0;

% compute right hand sides of the semi-discrete PDE
rhsu = rx.*(Dr*q) - LIFT*(Fscale.*(nx.*dq));
return
