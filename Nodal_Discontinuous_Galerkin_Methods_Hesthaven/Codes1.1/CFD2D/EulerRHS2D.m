function [rhsQ] = EulerRHS2D(Q,time, ExactSolutionBC);

% function [rhsQ] = EulerRHS2D(Q,time, ExactSolutionBC);
% Purpose: Evaluate RHS in 2D Euler equations, discretized on weak form
%             with a local Lax-Friedrich flux

Globals2D;

vmapM = reshape(vmapM, Nfp*Nfaces, K); vmapP = reshape(vmapP, Nfp*Nfaces, K);

% 1. Compute volume contributions (NOW INDEPENDENT OF SURFACE TERMS)
gamma = 1.4;
[F,G,rho,u,v,p] = EulerFluxes2D(Q, gamma);

% Compute weak derivatives
for n=1:4
  dFdr = Drw*F(:,:,n); dFds = Dsw*F(:,:,n);
  dGdr = Drw*G(:,:,n); dGds = Dsw*G(:,:,n);
  rhsQ(:,:,n) = (rx.*dFdr + sx.*dFds) + (ry.*dGdr + sy.*dGds);
end
    
% 2. Compute surface contributions 
% 2.1 evaluate '-' and '+' traces of conservative variables
for n=1:4
  Qn = Q(:,:,n);
  QM(:,:,n) = Qn(vmapM); QP(:,:,n) = Qn(vmapP);
end

% 2.2 set boundary conditions by modifying positive traces
if(~isempty(ExactSolutionBC))
  QP = feval(ExactSolutionBC, Fx, Fy, nx, ny, mapI, mapO, mapW, mapC, QP, time);
end

% 2.3 evaluate primitive variables & flux functions at '-' and '+' traces
[fM,gM,rhoM,uM,vM,pM] = EulerFluxes2D(QM, gamma);
[fP,gP,rhoP,uP,vP,pP] = EulerFluxes2D(QP, gamma);

% 2.4 Compute local Lax-Friedrichs/Rusonov numerical fluxes
lambda = max( sqrt(uM.^2+vM.^2) + sqrt(abs(gamma*pM./rhoM)),  ...
	      sqrt(uP.^2+vP.^2) + sqrt(abs(gamma*pP./rhoP)));
lambda = reshape(lambda, Nfp, Nfaces*K);
lambda = ones(Nfp, 1)*max(lambda, [], 1); 
lambda = reshape(lambda, Nfp*Nfaces, K);

% 2.5 Lift fluxes
for n=1:4
  nflux = nx.*(fP(:,:,n) + fM(:,:,n)) + ny.*(gP(:,:,n) + gM(:,:,n)) + ...
      lambda.*(QM(:,:,n) - QP(:,:,n));
  rhsQ(:,:,n) = rhsQ(:,:,n) - LIFT*(Fscale.*nflux/2);
end
return;

