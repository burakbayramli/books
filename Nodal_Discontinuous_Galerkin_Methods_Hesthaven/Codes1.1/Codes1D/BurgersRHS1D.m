function [rhsu] = BurgersRHS1D(u,epsilon,xL,xR,time)

% function [rhsu] = BurgersRHS1D(u,epsilon,xL, xR, time)
% Purpose  : Evaluate RHS flux in 1D viscous Burgers equation

Globals1D;

% Define field differences at faces
du = zeros(Nfp*Nfaces,K); du(:) = u(vmapM)-u(vmapP);

% impose boundary condition at x=0
uin =-tanh((xL+0.5-time)/(2*epsilon))+1.0; du(mapI) = 2.0*(u(vmapI)-uin);
uout=-tanh((xR+0.5-time)/(2*epsilon))+1.0; du(mapO) = 2.0*(u(vmapO)-uout);

% Compute q and jumps
q = sqrt(epsilon)*(rx.*(Dr*u) - LIFT*(Fscale.*(nx.*du/2.0)));
dq = zeros(Nfaces,K); dq(:) = (q(vmapM)-q(vmapP))/2.0;

% impose boundary condition - Dirichlet conditions
dq(mapI) = 0.0; dq(mapO) = 0.0;

% Evaluate nonlinear flux
du2 = zeros(Nfp*Nfaces,K); du2(:) = (u(vmapM).^2-u(vmapP).^2)/2.0;

% impose boundary condition
du2(mapI)=(u(vmapI).^2-uin.^2); du2(mapO)=(u(vmapO).^2-uout.^2);

% Compute flux
maxvel = max(max(abs(u)));

% penalty scaling -- See Chapter 7.2
%tau = .25*reshape(N*N./max(2*J(vmapP),2*J(vmapM)), Nfp*Nfaces, K);
tau=0;

% flux term
flux = nx.*(du2/2.0 - sqrt(epsilon)*dq) - maxvel/2.0.*du - sqrt(epsilon)*tau.*du;

% local derivatives of field
dfdr = Dr*(u.^2/2 - sqrt(epsilon)*q); 

% compute right hand sides of the semi-discrete PDE
rhsu = -(rx.*dfdr - LIFT*(Fscale.*flux));
return
