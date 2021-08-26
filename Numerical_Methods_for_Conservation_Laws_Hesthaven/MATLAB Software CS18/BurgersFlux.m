function [fu] = BurgersFlux(u);
% function [fu] = BurgersFlux(u);
% Purpose: Evaluate flux for Burgers
fu = u.^2;
return