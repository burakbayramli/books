function [Ux, Uy, PR] = PearsonVortexIC2D(x, y, time, nu)

% function [Ux, Uy, PR] = PearsonVortexIC2D(x, y, time, nu)
% Purpose: evaluate solution for channel flow with walls at y=0,2

Ux = -sin(2*pi*y).*exp(-nu*4*pi^2*time); Uy = sin(2*pi*x).*exp(-nu*4*pi^2*time);
PR = -cos(2*pi*x).*cos(2*pi*y).*exp(-nu*8*pi^2*time);
return


