function [rhsE, rhsH] = MaxwellRHS1D(E,H,eps,mu)

% function [rhsE, rhsH] = MaxwellRHS1D(E,H,eps,mu)
% Purpose  : Evaluate RHS flux in 1D Maxwell 

Globals1D;

% Compute impedance
Zimp = sqrt(mu./eps);

% Define field differences at faces
dE = zeros(Nfp*Nfaces,K); dE(:) = E(vmapM)-E(vmapP);
dH = zeros(Nfp*Nfaces,K); dH(:) = H(vmapM)-H(vmapP);
Zimpm = zeros(Nfp*Nfaces,K); Zimpm(:) = Zimp(vmapM);
Zimpp = zeros(Nfp*Nfaces,K); Zimpp(:) = Zimp(vmapP);
Yimpm = zeros(Nfp*Nfaces,K); Yimpm(:) = 1./Zimpm(:);
Yimpp = zeros(Nfp*Nfaces,K); Yimpp(:) = 1./Zimpp(:); 

% Homogeneous boundary conditions, Ez=0
Ebc = -E(vmapB); dE (mapB) = E(vmapB) - Ebc; 
Hbc =  H(vmapB); dH (mapB) = H(vmapB) - Hbc;

% evaluate upwind fluxes
fluxE = 1./(Zimpm + Zimpp).*(nx.*Zimpp.*dH - dE);
fluxH = 1./(Yimpm + Yimpp).*(nx.*Yimpp.*dE - dH);

% compute right hand sides of the PDE's
rhsE = (-rx.*(Dr*H) + LIFT*(Fscale.*fluxE))./eps;
rhsH = (-rx.*(Dr*E) + LIFT*(Fscale.*fluxH))./mu;
return
