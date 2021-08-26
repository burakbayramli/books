function [numflux] = EulerRoe(u,v,gamma,lambda,maxvel)
% function [numflux] = EulerRoe(u,v,gamma,lambda,maxvel);
% Purpose: Evaluate Roe numerical flux for Euler's equation. No entropy fix

% Compute flux for u
ru = u(:,1); ruu = u(:,2); Eu = u(:,3); uu = ruu./ru;
pu = (gamma-1)*(Eu - 0.5*ruu.^2./ru); Hu = (Eu+pu)./ru; 
fu = [ruu (ruu.^2./ru+pu) (Eu+pu).*ruu./ru];

% Compute flux for v
rv = v(:,1); ruv = v(:,2); Ev = v(:,3); uv = ruv./rv;
pv = (gamma-1)*(Ev - 0.5*ruv.^2./rv); Hv = (Ev+pv)./rv;
fv = [ruv (ruv.^2./rv+pv) (Ev+pv).*ruv./rv];

% Compute Roe averaged variables
uh = (sqrt(ru).*uu+sqrt(rv).*uv)./(sqrt(ru)+sqrt(rv));
Hh = (sqrt(ru).*Hu+sqrt(rv).*Hv)./(sqrt(ru)+sqrt(rv));
ch = sqrt((gamma-1)*(Hh - 0.5*uh.^2));

a2 = (gamma-1)*((rv-ru).*(Hh-uh.^2) + uh.*(ruv-ruu) - (Ev-Eu))./(ch.^2);
a1 = ((rv-ru).*(uh+ch) - (ruv-ruu) - ch.*a2)./(2*ch);
a3 = (rv-ru) - (a1+a2);

Unit = ones(length(uh),1);
s1 = [Unit uh-ch Hh-uh.*ch]; 
s2 = [Unit uh uh.^2/2]; 
s3 = [Unit uh+ch Hh+uh.*ch];

% Compute numerical flux
numflux = (fu+fv)/2 - 0.5*(bsxfun(@times,s1,a1.*abs(uh-ch)) + ...
       bsxfun(@times,s2,a2.*abs(uh)) + bsxfun(@times,s3,a3.*abs(uh+ch)));
return