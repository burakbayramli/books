function [numflux] = EulerLLF(u,v,gamma,lambda,maxvel)
% function [numflux] = Eulerflux(u,v,gamma,lambda,maxvel);
% Purpose: Evaluate the local Lax Friedrich numerical flux for 
% the Euler equations

% Compute flux for u
r = u(:,1); ru = u(:,2); E = u(:,3);
pu = (gamma-1)*(E - 0.5*ru.^2./r); cu = sqrt(gamma*pu./u(:,1));
fu = [ru (ru.^2./r+pu) (E+pu).*ru./r];

% Compute flux for v
r = v(:,1); ru = v(:,2); E = v(:,3);
pv = (gamma-1)*(E - 0.5*ru.^2./r); cv = sqrt(gamma*pv./v(:,1));
fv = [ru (ru.^2./r+pv) (E+pv).*ru./r];

% Evaluate numerical flux
localvel = max((cu+abs(u(:,2)./u(:,1))), (cv+abs(v(:,2)./v(:,1))));
numflux = (fu+fv)/2 - bsxfun(@times, v-u, localvel)/2; 
return