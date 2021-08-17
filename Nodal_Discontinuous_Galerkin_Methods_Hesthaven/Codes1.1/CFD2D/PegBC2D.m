function Q = PegBC2D(xin, yin, nxin, nyin, mapI, mapO, mapW, mapC, Q, time);
  
%  function [Q] = PegBC2D(xin, yin, nxin, nyin, mapI, mapO, mapW, Q, time);
% Purpose: Impose wall and cylinder boundary conditions

% Example is Mach ** 0.4 ** flow in wind tunnel
gamma = 1.4; pin = 1.0; rhoin = 1.4;

% extract conserved variables
rho = Q(:,:,1); rhou = Q(:,:,2); rhov = Q(:,:,3); Ener = Q(:,:,4);

% Wall conditions -- reflective, isothermal, i.e., n.u=0, T=T(t=0)
Temp  = pin/rhoin/(gamma-1);
rhoW = rho(mapW); rhouW = rhou(mapW); rhovW = rhov(mapW); EnerW = Ener(mapW);
nxW = nxin(mapW); nyW = nyin(mapW);

rhou(mapW) = -rhou(mapW); rhov(mapW) = -rhov(mapW);
Ener(mapW) = rhoW*Temp + 0.5*(rhou(mapW).^2 + rhov(mapW).^2)./rhoW;

% cylinder conditions -- isothermal, rotating nonslip
Temp  = pin/rhoin/(gamma-1);
rhoC = rho(mapC); rhouC = rhou(mapC); rhovC = rhov(mapC); EnerC = Ener(mapC);
nxC = nxin(mapC); nyC = nyin(mapC);

rhou(mapC) = sin(pi*time)*( yin(mapC)); rhov(mapC) = sin(pi*time)*(-xin(mapC));
Ener(mapC) = rhoC*Temp + 0.5*(rhou(mapC).^2 + rhov(mapC).^2)./rhoC;

% pack modified conserved variables
Q(:,:,1) = rho; Q(:,:,2) = rhou; Q(:,:,3) = rhov; Q(:,:,4) = Ener;
return
