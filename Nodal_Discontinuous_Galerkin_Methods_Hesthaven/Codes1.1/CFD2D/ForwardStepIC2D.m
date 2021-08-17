function Q = ForwardStepIC2D(x2d, y2d, time)
 
% function Q = ForwardStepIC2D(x2d, y2d, time)
% Purpose: compute plane flow configuration 

Globals2D;

gamma = 1.4;

rho = gamma*ones(Np,K);  p = ones(Np,K); 

% M = |u|/c,  c = sqrt(gamma*p/rho)
rhou = rho.*(3*ones(Np, K)); rhov = zeros(Np, K); 
Ener = p/(gamma-1.0) + (rhou.^2+rhov.^2)./(2*rho);

Q(:,:,1) = rho; Q(:,:,2) = rhou; Q(:,:,3) = rhov; Q(:,:,4) = Ener; 
return
