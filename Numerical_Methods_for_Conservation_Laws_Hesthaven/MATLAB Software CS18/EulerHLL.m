function [numflux] = EulerHLL(u,v,gamma,lambda,maxvel)
% function [numflux] = EulerHLL(u,v,gamma,lambda,maxvel);
% Purpose: Evaluate HLL numerical flux for Euler's equation

% Compute flux for u
r = u(:,1); ru = u(:,2); E = u(:,3); pu = (gamma-1)*(E - 0.5*ru.^2./r);
fu = [ru (ru.^2./r+pu) (E+pu).*ru./r];
cu = sqrt(gamma*pu./r); uu = ru./r; sm = min([uu-cu uu uu+cu], [], 2);

% Compute flux for v
r = v(:,1); ru = v(:,2); E = v(:,3); pv = (gamma-1)*(E - 0.5*ru.^2./r);
fv = [ru (ru.^2./r+pv) (E+pv).*ru./r];
cv = sqrt(gamma*pv./r); uv = ru./r; sp = max([uv-cv uv uv+cv],[],2);

% Compute Roe average and extreme velocities
us = bsxfun(@times,(bsxfun(@times,v,sp) - ...
        bsxfun(@times,u,sm)+fu-fv),1./(sp-sm));
r = us(:,1); ru = us(:,2); E = us(:,3); 
ps = (gamma-1)*(E - 0.5*ru.^2./r); cs = sqrt(gamma*ps./r); uus = ru./r;
sm = min([sm uus-cs uus uus+cs],[],2); sp = max([sp uus-cs uus uus+cs],[],2);

% Compute numerical flux
fs = bsxfun(@times,(bsxfun(@times,fu,sp)-bsxfun(@times,fv,sm)+ ...
         bsxfun(@times,v-u,sp.*sm)),1./(sp-sm));
numflux = bsxfun(@times,fu,(sm>0))+ ...
         bsxfun(@times,fs,((sm<=0).*(sp>=0)))+bsxfun(@times,fv,(sp<0));
return