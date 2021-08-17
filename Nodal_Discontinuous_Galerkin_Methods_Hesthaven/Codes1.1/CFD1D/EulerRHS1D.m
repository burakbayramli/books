function [rhsrho, rhsrhou, rhsEner] = EulerRHS1D(rho, rhou ,Ener)

% function [rhsrho, rhsrhou, rhsEner] = EulerRHS1D(rho, rhou ,Ener)
% Purpose  : Evaluate RHS flux in 1D Euler

Globals1D;

% compute maximum velocity for LF flux
gamma = 1.4;
pres = (gamma-1.0)*(Ener - 0.5*(rhou).^2./rho);
cvel = sqrt(gamma*pres./rho); lm = abs(rhou./rho)+cvel;

% Compute fluxes
rhof = rhou; rhouf=rhou.^2./rho+pres; Enerf=(Ener+pres).*rhou./rho;

% Compute jumps at internal faces
drho  =zeros(Nfp*Nfaces,K);  drho(:)  =  rho(vmapM)-  rho(vmapP); 
drhou =zeros(Nfp*Nfaces,K); drhou(:)  = rhou(vmapM)- rhou(vmapP);
dEner =zeros(Nfp*Nfaces,K); dEner(:)  = Ener(vmapM)- Ener(vmapP);
drhof =zeros(Nfp*Nfaces,K); drhof(:)  = rhof(vmapM)- rhof(vmapP);
drhouf=zeros(Nfp*Nfaces,K); drhouf(:) =rhouf(vmapM)-rhouf(vmapP);
dEnerf=zeros(Nfp*Nfaces,K); dEnerf(:) =Enerf(vmapM)-Enerf(vmapP);
LFc   =zeros(Nfp*Nfaces,K); LFc(:)    =max(lm(vmapP),lm(vmapM));

% Compute fluxes at interfaces
drhof(:) = nx(:).*drhof(:)/2.0-LFc(:)/2.0.*drho(:); 
drhouf(:)=nx(:).*drhouf(:)/2.0-LFc(:)/2.0.*drhou(:); 
dEnerf(:)=nx(:).*dEnerf(:)/2.0-LFc(:)/2.0.*dEner(:); 

% Boundary conditions for Sod's problem
rhoin    = 1.000;   rhouin   = 0.0;
pin      = 1.000;   Enerin   = pin/(gamma-1.0);
rhoout   = 0.125;   rhouout  = 0.0;
pout     = 0.100;   Enerout  = pout/(gamma-1.0);

% Set fluxes at inflow/outflow
rhofin =rhouin; rhoufin=rhouin.^2./rhoin+pin; 
Enerfin=(pin/(gamma-1.0)+0.5*rhouin^2/rhoin+pin).*rhouin./rhoin;
lmI=lm(vmapI)/2; nxI=nx(mapI);
drhof (mapI)=nxI*(rhof (vmapI)-rhofin )/2.0-lmI*(rho(vmapI) -rhoin);  
drhouf(mapI)=nxI*(rhouf(vmapI)-rhoufin)/2.0-lmI*(rhou(vmapI)-rhouin);
dEnerf(mapI)=nxI*(Enerf(vmapI)-Enerfin)/2.0-lmI*(Ener(vmapI)-Enerin);

rhofout=rhouout; rhoufout=rhouout.^2./rhoout+pout; 
Enerfout=(pout/(gamma-1.0)+0.5*rhouout^2/rhoout+pout).*rhouout./rhoout;
lmO=lm(vmapO)/2; nxO=nx(mapO);
drhof (mapO)=nxO*(rhof(vmapO) - rhofout)/2.0-lmO*(rho (vmapO)- rhoout);  
drhouf(mapO)=nxO*(rhouf(vmapO)-rhoufout)/2.0-lmO*(rhou(vmapO)-rhouout);
dEnerf(mapO)=nxO*(Enerf(vmapO)-Enerfout)/2.0-lmO*(Ener(vmapO)-Enerout);

% compute right hand sides of the PDE's
rhsrho  = -rx.*(Dr*rhof)  + LIFT*(Fscale.*drhof);
rhsrhou = -rx.*(Dr*rhouf) + LIFT*(Fscale.*drhouf);
rhsEner = -rx.*(Dr*Enerf) + LIFT*(Fscale.*dEnerf);
return
