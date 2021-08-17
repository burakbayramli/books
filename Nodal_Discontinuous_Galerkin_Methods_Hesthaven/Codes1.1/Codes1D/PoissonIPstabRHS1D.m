function [rhsu] = PoissonIPstabRHS1D(u)

% function [rhsu] = PoissonIPstabRHS1D(u)
% Purpose  : Evaluate RHS in 1D Poisson equation on symmetric form
%            using stabilized internal penalty flux

Globals1D;

% Define field differences at faces
du  = zeros(Nfp*Nfaces,K); du(:)  = u(vmapM)-u(vmapP);

% impose boundary condition -- Dirichlet BC's
uin  = -u(vmapI); du(mapI) = u(vmapI)-uin; 
uout = -u(vmapO); du(mapO) = u(vmapO)-uout;

% Compute q
fluxu = nx.*du/2.0;
ux = rx.*(Dr*u);
q = ux - LIFT*(Fscale.*fluxu);
dq = zeros(Nfp*Nfaces,K); dq(:) = q(vmapM)-(ux(vmapM)+ux(vmapP))/2.0;

% impose boundary condition -- Neumann BC's
qin  = ux(vmapI); dq (mapI) = q(vmapI) - (ux(vmapI)+ qin )/2.0;
qout = ux(vmapO); dq (mapO) = q(vmapO) - (ux(vmapO)+ qout)/2.0;

% evaluate fluxes
hmin = 2.0/max(max(rx)); tau = Np^2/hmin;
fluxq = nx.*(dq+tau*nx.*du);

% compute right hand sides of the semi-discrete PDE
rhsu = J.*((invV'*invV)*(rx.*(Dr*q) - LIFT*(Fscale.*fluxq)));
return
