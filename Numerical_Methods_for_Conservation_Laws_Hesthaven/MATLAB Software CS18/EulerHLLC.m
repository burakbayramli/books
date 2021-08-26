function [numflux] = EulerHLLC(u,v,gamma,lambda,maxvel)
% function [numflux] = EulerHLLC(u,v,gamma,lambda,maxvel);
% Purpose: Evaluate HLLC numerical flux for Euler's equation. 

% Compute flux for u
ru = u(:,1); ruu = u(:,2); E = u(:,3); pu = (gamma-1)*(E - 0.5*ruu.^2./ru);
fu = [ruu (ruu.^2./ru+pu) (E+pu).*ruu./ru];
cu = sqrt(gamma*pu./ru); uu = ruu./ru; sm = min([uu-cu uu uu+cu],[],2);

% Compute flux for v
rv = v(:,1); ruv = v(:,2); E = v(:,3); pv = (gamma-1)*(E - 0.5*ruv.^2./rv);
fv = [ruv (ruv.^2./rv+pv) (E+pv).*ruv./rv];
cv = sqrt(gamma*pv./rv); uv = ruv./rv; sp = max([uv-cv uv uv+cv],[],2);

% Compute Roe pressure
ss = (pv-pu + ruu.*(sm-uu) - ruv.*(sp-uv))./(ru.*(sm-uu)-rv.*(sp-uv));
Plr = 0.5*(pu+pv+ru.*(sm-uu).*(ss-uu) + rv.*(sp-uv).*(ss-uv));

% Compute numerical flux
Ds = 0*u; Ds(:,2)=1; Ds(:,3)=ss;
fus = bsxfun(@times,bsxfun(@times,bsxfun(@times,u,sm)-fu,ss)+ ...
         bsxfun(@times,Ds,Plr.*sm),1./(sm-ss));
fvs = bsxfun(@times,bsxfun(@times,bsxfun(@times,v,sp)-fv,ss)+ ...
         bsxfun(@times,Ds,Plr.*sp),1./(sp-ss));
numflux = bsxfun(@times,fu,(sm>=0))+bsxfun(@times,fus,((sm<=0).*(ss>0)))+ ...
         bsxfun(@times,fvs,((ss<=0).*(sp>=0)))+bsxfun(@times,fv,(sp<=0));
return