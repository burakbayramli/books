function [ulimit] = MomentLimitDG(x,u,m,h,N,V,iV);
% function ulimit = MomentLimitDG(x,u,m,h,N,V,iV);
% Purpose: Apply moment limiter to u - an m'th order polynomial            
eps0=1.0e-8; eps1 = 1.0e-8;

% Strength of slope limiter - Minmod: theta=1, MUSCL: theta=2
theta=2.0;

% Compute cell averages and cell centers
uh = iV*u; uh(2:(m+1),:)=0; uavg = V*uh; ucell = uavg(1,:); ulimit = u; 

% Extend cell averages
[ve] = extendDG(ucell,'N',0,'N',0);

% extract end values and cell averages for each element
uel = u(1,:); uer = u(end,:); vj = ucell; vjm = ve(1:N); vjp = ve(3:N+2);

% Find elements that require limiting
vel = vj - minmod([(vj-uel)' (vj-vjm)' (vjp-vj)'])';
ver = vj + minmod([(uer-vj)' (vj-vjm)' (vjp-vj)'])';
ids = (abs(vel-uel)<eps1 & abs(ver-uer)<eps1); 
mark = zeros(1,N); mark = (ids | mark);

% Compute expansion coefficients
uh = iV*u; 

% Apply limiting when needed
for i=m+1:-1:2
  uh1 = uh(i,:); uh2 = uh(i-1,:);
  [uh2e] = extendDG(uh2,'P',0,'P',0); uh2m = uh2e(1:N); uh2p = uh2e(3:N+2);
  con = sqrt((2*i+1)*(2*i-1));
  uh1 = 1/con*minmod([con*uh1' theta*(uh2p - uh2)'  ...
           theta*(uh2 - uh2m)'])'.*(1-mark) + mark.*uh1;
  idsh = abs(uh1-uh(i,:))<eps0; mark = (idsh | mark);
  uh(i,:) = uh1;
end
ulimit = V*uh;
return