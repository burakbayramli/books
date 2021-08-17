function [ux,uy] = Grad2D(u);

% function [ux,uy] = Grad2D(u);
% Purpose: Compute 2D gradient field of scalar u

Globals2D;

ur = Dr*u; us = Ds*u;
ux = rx.*ur + sx.*us; uy = ry.*ur + sy.*us;
return
