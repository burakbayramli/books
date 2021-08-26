function [fu] = BurgersJac2Dy(u)
% function [fu] = BurgersJac2Dy(u)
% Purpose: Evaluate y-component of flux for Jacobian 2D Burgers
fu = 2*u;
return