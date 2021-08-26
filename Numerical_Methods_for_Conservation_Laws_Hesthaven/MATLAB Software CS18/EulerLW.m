function [numflux] = EulerLW(u,v,gamma,lambda,maxvel)
% function [numflux] = EulerLW(u,v,gamma,lambda,maxvel);
% Purpose: Evaluate Lax Wendroff numerical flux for the Euler equations

% Compute flux for u
r = u(:,1); ru = u(:,2); E = u(:,3); pu = (gamma-1)*(E - 0.5*ru.^2./r);
fu = [ru (ru.^2./r+pu) (E+pu).*ru./r];

% Compute flux for v
r = v(:,1); ru = v(:,2); E = v(:,3); pv = (gamma-1)*(E - 0.5*ru.^2./r);
fv = [ru (ru.^2./r+pv) (E+pv).*ru./r];

% Evaluate numerical flux
fw = fv-fu; w = (u+v)/2; rw = w(:,1); ruw = w(:,2); 
Ew = w(:,3); uw = ruw./rw; wL = length(rw); 
A21 = -(3-gamma)/2*uw.^2; A22 = (3-gamma)*uw; A23 = (gamma-1)*ones(wL,1);
A31 = -gamma*Ew.*uw./rw + (gamma-1)*uw.^3;
A32 = gamma*Ew./rw - 3*(gamma-1)/2*uw.^2; A33 = gamma*uw;
LFvec = zeros(wL,3); LFvec(:,1) = fw(:,2); 
LFvec(:,2) = A21.*fw(:,1)+A22.*fw(:,2)+A23.*fw(:,3);
LFvec(:,3) = A31.*fw(:,1)+A32.*fw(:,2)+A33.*fw(:,3);
numflux = ((fu+fv) - lambda*LFvec)/2;
return