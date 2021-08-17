function [bcUx, bcUy, bcPR, bcdUndt] = PearsonVortexBC2D(x, y, nx, ny, mapI, mapO, mapW, mapC, time, nu)

% function [bcUx, bcUy, bcPR, bcdUdn] = PearsonVortexBC2D(x, y, nx, ny, mapI, mapO, mapW, time, nu)
% Purpose: evaluate boundary conditions for Pearson's vortex exact solution 

bcUx = zeros(size(x)); bcUy = zeros(size(x)); bcPR = zeros(size(x)); bcdUndt = zeros(size(x));

time = 0;

% Dirichlet velocity, condition on inflow
xI = x(mapI); yI = y(mapI); nxI = nx(mapI); nyI = ny(mapI);
bcUx(mapI)= -sin(2*pi*yI).*exp(-nu*4*pi^2*time); bcUy(mapI)=  sin(2*pi*xI).*exp(-nu*4*pi^2*time);
bcdUndt(mapI) = (-nxI.*sin(2*pi*yI)+nyI.*sin(2*pi*xI) ).*exp(-nu*4*pi^2*time);

% Neumann velocity, Dirichlet pressure, condition on outflow
xO = x(mapO); yO = y(mapO); nxO = nx(mapO); nyO = ny(mapO);
bcUx(mapO)= nyO.*(( 2*pi).*(-cos(2*pi*yO).*exp(-nu*4*pi^2*time)));
bcUy(mapO)= nxO.*(( 2*pi).*( cos(2*pi*xO).*exp(-nu*4*pi^2*time)));
bcPR(mapO) = -cos(2*pi*xO).*cos(2*pi*yO).*exp(-nu*8*pi^2*time);
return


