function [numflux] = MaxwellUpwind(u,v,epu,epv,muu,muv,ep,mu)
% function [numflux] = MaxwellUpwind(u,v,epu,epv,muu,muv,ep,mu)
% Purpose: Evaluate exact upwind numerical flux for Maxwell's equation with
% discontinuous coefficients

Zm = sqrt(muu./epu); Zp = sqrt(muv./epv); Zavg = (Zm+Zp)./2;
Ym = 1./Zm; Yp = 1./Zp; Yavg = (Ym+Yp)./2;

Hs = ( (Zm.*u(:,2) + Zp.*v(:,2))/2 + (u(:,1)-v(:,1))/2 )./Zavg;
Es = ( (Ym.*u(:,1) + Yp.*v(:,1))/2 + (u(:,2)-v(:,2))/2 )./Yavg;
numflux = [Hs./ep Es./mu];
end