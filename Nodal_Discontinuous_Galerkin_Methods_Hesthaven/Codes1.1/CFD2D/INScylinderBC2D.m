function [bcUx, bcUy, bcPR, bcdUndt] = INScylinderBC2D(x, y, nx, ny, mapI, mapO, mapW, mapC, time, nu)

% function [bcUx, bcUy, bcPR, bcdUndt] = INScylinderBC2D(x, y, nx, ny, mapI, mapO, mapW, mapC, time, nu)
% Purpose: evaluate boundary conditions for channel bounded cylinder flow with walls at y=+/- .15

% TEST CASE: from 
% V. John "Reference values for drag and lift of a two-dimensional time dependent flow around a cylinder", 
% Int. J. Numer. Meth. Fluids 44, 777 - 788, 2004

bcUx = zeros(size(x)); bcUy = zeros(size(x)); bcPR = zeros(size(x)); bcdUndt = zeros(size(x));

% inflow
yI = y(mapI)+.20;
bcUx(mapI)= (1/.41)^2*6*yI.*(0.41 - yI); bcUy(mapI)= 0;
bcdUndt(mapI) = -(1/.41)^2*6*yI.*(0.41 - yI);

% wall
bcUx(mapW)= 0; bcUy(mapW)= 0;

% cylinder
bcUx(mapC)= 0; bcUy(mapC)= 0;

% outflow (Neumann for velocity)
yO = y(mapO)+.20;
bcUx(mapO)= 0; bcUy(mapO)= 0; bcdUndt(mapO) = 0;
