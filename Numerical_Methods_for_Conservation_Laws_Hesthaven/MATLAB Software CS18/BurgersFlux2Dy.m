function [fu] = BurgersFlux2Dy(u)
% function [fu] = BurgersFlux2Dy(u)
% Purpose: Evaluate y-component of flux for 2D Burgers
fu = u.^2;
return