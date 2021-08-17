function [rhsu] = AdvecRHS3D(u,time)

% function [rhsu] = AdvecRHS3D(u,time)
% Purpose  : Evaluate RHS flux in 3D advection

Globals3D;

% form field differences at faces
alpha=1; du = zeros(Nfp*Nfaces,K); 
du(:) = 0.5*(u(vmapM)-u(vmapP)).*(nx(:) - alpha*abs(nx(:)));

% impose boundary condition at x=0
ubc  = exp(-1*( (Fx(mapB)-time).^2 + Fy(mapB).^2 + Fz(mapB).^2));
du(mapB) = 0.5*(u(vmapB)-ubc).*(nx(mapB)-alpha*abs(nx(mapB)));

% compute right hand sides of the semi-discrete PDE
rhsu = -(rx.*(Dr*u)+sx.*(Ds*u)+tx.*(Dt*u)) + LIFT*(Fscale.*(du));
return
