function [fu] = BurgersFlux2Dx(u)
% function [fu] = BurgersFlux2Dx(u)
% Purpose: Evaluate x-component of flux for 2D Burgers
fu = u.^2;
return