function [rho,rhou,rhov,Ener] = ChannelIC2D(x, y, time);
  
%  function [Q] = ChannelIC2D(x, y, time)
%  Purpose: Impose uniform plane flow 

mu = 1e-2; pbar = 10; gamma = 1.5;

rho  = 1;
rhou = y.^2;
rhov = 0;
Ener = (2*mu*x + pbar)/(gamma-1) + .5*(y.^4);
return
