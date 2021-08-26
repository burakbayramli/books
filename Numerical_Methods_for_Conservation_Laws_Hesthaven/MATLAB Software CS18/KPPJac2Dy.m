function [fu] = KPPJac2Dy(u)
% function [fu] = KPPJac2Dy(u)
% Purpose: Evaluate y-component of flux for Jacobian 2D KPP equation
fu = -sin(u);
return