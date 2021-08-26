function [ulimit] = SlopeLimitBSBDG(x,u,m,h,N,V,iV);
% function ulimit = SlopeLimitBSBDG(x,u,m,h,N,V,iV);
% Purpose: Apply slopelimiter by Burbeau-Sagaut-Bruneau (2001)
% to u - an m'th order polynomial            
eps0=1.0e-8;

% Strength of slope limiter - Minmod: theta=1, MUSCL: theta=2
theta=2.0;

% Compute cell averages and cell centers
uh = iV*u; uhx = uh; uh(2:(m+1),:)=0; uavg = V*uh; ucell = uavg(1,:); 
uhx(3:(m+1),:)=0; ulin = V*uhx; ux = 2/h*(ucell - ulin(1,:)); ulimit = u; 

% Extend cell averages
[ve] = extendDG(ucell,'P',0,'P',0); [vxe] = extendDG(ux,'P',0,'P',0);

% extract end values and cell averages for each element
uel = u(1,:); uer = u(end,:); vj = ucell; vjm = ve(1:N); vjp = ve(3:N+2);
vxj = ux; vxjm = vxe(1:N); vxjp = vxe(3:N+2);

% Find elements that require limiting
vel = vj - minmod([(vj-uel)' (vj-vjm)' (vjp-vj)'])';
ver = vj + minmod([(uer-vj)' (vj-vjm)' (vjp-vj)'])';
ids = find(abs(vel-uel)>eps0 | abs(ver-uer)>eps0); 

% Apply limiting when needed
if(~isempty(ids))
  % create piecewise linear solution for limiting on specified elements
  uhl = iV*u(:,ids); uhl(3:(m+1),:)=0; ulin = V*uhl;
  x0h=ones(m+1,1)*(x(end,:)+x(1,:))/2;

  % Limit function  
  ux1 = minmod([vxj(ids)' theta*(vjp(ids)-vj(ids))'./h ...
    theta*(vj(ids)-vjm(ids))'./h])';
  ux2 = minmod([vxj(ids)' vxjm(ids)' vxjp(ids)'])';
  ulimit(:,ids) = ones(m+1,1)*vj(ids)+(x(:,ids)-x0h(:,ids)).*...
      (ones(m+1,1)*maxmod([ux1' ux2'])');     
end
return