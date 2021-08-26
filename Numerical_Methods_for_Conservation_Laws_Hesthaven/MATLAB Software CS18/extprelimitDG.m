function ulimit = extprelimitDG(x,u,m,h,N,V,iV,umin,umax);
% function ulimit = extprelimitDG(x,u,m,h,N,V,iV,umin,umax);
% Purpose: Apply extrema preserving limiter to piecewise
% polynomial solution u - an m'th order polynomial            
eps0=1.0e-8;

% Compute cell averages and cell centers
uh = iV*u; uh(2:(m+1),:)=0; uavg = V*uh; ucell = uavg(1,:); ulimit = u; 

% Extend cell averages
[ve] = extendDG(ucell,'P',0,'P',0);

% extract end values and cell averages for each element
uel = u(1,:); uer = u(end,:); 
vj = ucell; vjm = ve(1:N); vjp = ve(3:N+2);

% Find elements that require limiting
vel = vj - minmod([(vj-uel);(vj-vjm);(vjp-vj)]);
ver = vj + minmod([(uer-vj);(vj-vjm);(vjp-vj)]);
ids = find(abs(vel-uel)>eps0 | abs(ver-uer)>eps0); 

% Apply limiting when needed
if(~isempty(ids))
  minu = min(u(:,ids),[],1); maxu = max(u(:,ids),[],1);
  ulimmax = abs((umax-ucell(ids))./(maxu-ucell(ids)));
  ulimmin = abs((umin-ucell(ids))./(minu-ucell(ids)));
  theta = min([ulimmax; ulimmin; ones(1,length(ids))],[],1);
  ulimit(:,ids) = (ones(m+1,1)*theta).*(u(:,ids)-uavg(:,ids)) + uavg(:,ids);
end
return