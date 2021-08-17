function ulimit = SlopeLimitN(u);

% function ulimit = SlopeLimitN(u);
% Purpose: Apply slopelimiter (Pi^N) to u assuming u an N'th order polynomial            

Globals1D;

% Compute cell averages
uh = invV*u; uh(2:Np,:)=0; uavg = V*uh; v = uavg(1,:);

% Apply slope limiter as needed.
ulimit = u; eps0=1.0e-8;

% find end values of each element
ue1 = u(1,:); ue2 = u(end,:);

% find cell averages
vk = v; vkm1 = [v(1),v(1:K-1)]; vkp1 = [v(2:K),v(K)]; 

% Apply reconstruction to find elements in need of limiting
ve1 = vk - minmod([(vk-ue1);vk-vkm1;vkp1-vk]);
ve2 = vk + minmod([(ue2-vk);vk-vkm1;vkp1-vk]);
ids = find(abs(ve1-ue1)>eps0 | abs(ve2-ue2)>eps0);

% Check to see if any elements require limiting
if(~isempty(ids))
  % create piecewise linear solution for limiting on specified elements
  uhl = invV*u(:,ids); uhl(3:Np,:)=0; ul = V*uhl;
  
  % apply slope limiter to selected elements
  ulimit(:,ids) = SlopeLimitLin(ul,x(:,ids),vkm1(ids),vk(ids),vkp1(ids));
end
return;
