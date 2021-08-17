function flux = EulerLF2D(lnx, lny, QM, QP, gamma)

% Function flux = EulerLF2D(nx, ny, QM, QP, gamma)
% Purpose: compute Local Lax-Friedrichs/Rusonov fluxes for Euler equations

Globals2D;

% Evaluate primitive variables & flux functions at '-' and '+' traces
[fxM,fyM,rhoM,uM,vM,pM] = EulerFluxes2D(QM, gamma);
[fxP,fyP,rhoP,uP,vP,pP] = EulerFluxes2D(QP, gamma);

% Compute wave speed for  Lax-Friedrichs/Rusonov numerical fluxes
maxvel = max( sqrt(uM.^2+vM.^2) + sqrt(abs(gamma*pM./rhoM)),  ...
	      sqrt(uP.^2+vP.^2) + sqrt(abs(gamma*pP./rhoP)));
      
NGauss = size(nx, 1)/Nfaces;
maxvel = reshape(maxvel, NGauss, Nfaces*K);
maxvel = ones(NGauss, 1)*max(maxvel, [], 1); 
maxvel = reshape(maxvel, NGauss*Nfaces, K);

% Compute + lift fluxes to volume residual
flux = zeros(size(fxP));
for n=1:4
  flux(:,:,n) = 0.5*(lnx.*(fxP(:,:,n) + fxM(:,:,n)) + lny.*(fyP(:,:,n) + fyM(:,:,n)) + ...
      maxvel.*(QM(:,:,n) - QP(:,:,n)));
end
return;
