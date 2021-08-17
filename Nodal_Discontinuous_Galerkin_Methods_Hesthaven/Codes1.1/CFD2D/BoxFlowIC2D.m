function [rho, rhou, rhov, Ener] = BoxFlowIC2D(x, y, time)
  
% function Q = BoxFlowIC2D(x, y, time)
% Purpose: compute plane flow configuration 

gamma = 1.4; pbar = 10;
Np = size(x,1); K = size(x,2);

if(1)

  pref = 12;
  rho = ones(Np, K);
  rhou = -sin(2*pi*y);
  rhov =  sin(4*pi*x);
  Ener = pref/(gamma-1) + 0.5*(rhou.^2 + rhov.^2)./rho;
else

  pref = 12;
  rho  = ones(Np,K);
  rhou = zeros(Np,K);
  rhov = zeros(Np,K);
  Ener = pref/(gamma-1) + 0.5*rho.*exp(-4*(cos(pi*x).^2 + cos(pi*y).^2));
end
return;
