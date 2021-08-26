function [fu] = BurgersJac2Dx(u)
% function [fu] = BurgersJac2Dx(u)
% Purpose: Evaluate x-component of flux for Jacobian 2D Burgers
fu = 2*u;
return