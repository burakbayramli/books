function [rho,rhou,rhov,Ener] = ChannelBC2D(rho, rhou, rhov, Ener, time)
  
% function [rho,rhou,rhov,Ener] = ChannelBC2D(rho, rhou, rhov, Ener, time)
% Purpose: Impose channel boundary conditions on 2D Euler equations on weak form

Globals2D;
gamma = 1.5; mu = 1e-2; pbar = 10;

xB = gauss.x(gauss.mapB); yB = gauss.y(gauss.mapB);

% Quadratic shear flow, relies on gamma=1.5
rho(gauss.mapB)  = 1;
rhou(gauss.mapB) = yB.^2;
rhov(gauss.mapB) = 0;
Ener(gauss.mapB) = (2*mu*xB + pbar)/(gamma-1) + .5*(yB.^4);
return
