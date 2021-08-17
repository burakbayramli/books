function [Q] = CouetteBC2D(xin, yin, nxin, nyin, mapI, mapO, mapW, mapC, Q, time);

% function [Q] = CouetteBC2D(xin, yin, nxin, nyin, mapI, mapO, mapW, mapC, Q, time);
% Purpose: evaluate solution for Couette flow

% Couette flow (mach .2 at inner cylinder)
gamma = 1.4;

% extract conserved variables
rho = Q(:,:,1); rhou = Q(:,:,2); rhov = Q(:,:,3); Ener = Q(:,:,4);

mapB = [mapI;mapO];

rad = sqrt(xin(mapB).^2 + yin(mapB).^2);
theta = atan2(yin(mapB), xin(mapB));

utheta = (-rad + 16./rad)/75;
p = 1 + (1./(75^2))*( (rad.^2)/2 - 32*log(rad) - 128./rad.^2 );

rho (mapB) = 1;
rhou(mapB) = -sin(theta).*utheta;
rhov(mapB) =  cos(theta).*utheta;
Ener(mapB) = p/(gamma-1) + 0.5*(rhou(mapB).^2 + rhov(mapB).^2)./rho(mapB);

% pack modified conserved variables
Q(:,:,1) = rho; Q(:,:,2) = rhou; Q(:,:,3) = rhov; Q(:,:,4) = Ener;
return
