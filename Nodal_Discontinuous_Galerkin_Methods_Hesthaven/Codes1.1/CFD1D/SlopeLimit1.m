function ulimit = SlopeLimit1(u);

% function ulimit = SlopeLimit1(u);
% Purpose: Apply slopelimiter (Pi^1) to u

Globals1D;
ulimit = zeros(Np,K);

% Compute modal coefficients
uh = invV*u; 

% Extract linear polynomial
ul = uh; ul(3:Np,:) = 0;ul = V*ul;

% Extract cell averages
uh(2:Np,:)=0; uavg = V*uh; v = uavg(1,:);

% Find cell averages in neighborhood of each element
vk = v; vkm1 = [v(1),v(1:K-1)]; vkp1 = [v(2:K),v(K)]; 

% Limit function in all cells
ulimit = SlopeLimitLin(ul,x,vkm1,vk,vkp1);
return
