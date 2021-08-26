function [fu] = KPPJac2Dx(u)
% function [fu] = KPPJac2Dx(u)
% Purpose: Evaluate x-component of flux for Jacobian 2D KPP equation
fu = cos(u);
return