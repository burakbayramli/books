function [fu] = KPPFlux2Dx(u)
% function [fu] = KPPFlux2Dx(u)
% Purpose: Evaluate x-component of flux for 2D KPP equation
fu = sin(u);
return