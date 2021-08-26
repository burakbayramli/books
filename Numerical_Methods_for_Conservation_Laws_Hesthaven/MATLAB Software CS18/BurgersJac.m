function [fu] = BurgersJac(u);
% function [fu] = BurgersJac(u);
% Purpose: Evaluate Jacobian for Burgers flux
fu = 2*u;
return