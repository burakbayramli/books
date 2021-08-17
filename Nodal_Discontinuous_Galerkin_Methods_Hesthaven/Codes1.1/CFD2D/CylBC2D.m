function [rho,rhou,rhov,Ener] = CylBC2D(rho, rhou, rhov, Ener, time)
  
% function [rho,rhou,rhov,Ener] = CylBC2D(rho, rhou, rhov, Ener, time)
% Purpose: Impose channel boundary conditions on 2D CNS equations on weak form

Globals2D;

% Example is Mach ** 0.4 ** flow in wind tunnel
gamma = 1.4;

gnx = gauss.nx; gny = gauss.ny; gmapI = gauss.mapI; gmapW = gauss.mapW; 
gmapC = gauss.mapC; gx = gauss.x; gy = gauss.y;

% Inflow conditions -- uniform inflow
rhoin = 1.4; uin = 0.4; vin = 0.0; pin = 1.0;
Ein = pin/(gamma-1.0) + 0.5*rhoin*(uin^2+vin^2);

yI = gy(gmapI)+.20;
rho(gmapI) = rhoin; 
rhou(gmapI)= rhoin*(1/.41)^2*6*yI.*(0.41 - yI);
rhov(gmapI)= 0;
Ener(gmapI) = Ein + 0.5*(rhou(gmapI).^2 + rhov(gmapI).^2)/rhoin;

% Outflow conditions -- supersonic outflow ( do nothing )

% Wall conditions -- reflective, isothermal, i.e., n.u=0, T=T(t=0)
Temp  = pin/rhoin/(gamma-1);
rhoW = rho(gmapW); rhouW = rhou(gmapW); rhovW = rhov(gmapW); EnerW = Ener(gmapW);
nxW = gnx(gmapW); nyW = nyin(gmapW);

rhou(gmapW) = -rhou(gmapW); rhov(gmapW) = -rhov(gmapW);
Ener(gmapW) = rhoW*Temp + 0.5*(rhou(gmapW).^2 + rhov(gmapW).^2)./rhoW;

% cylinder conditions -- reflective, isothermal, i.e., n.u=0, T=T(t=0)
Temp  = pin/rhoin/(gamma-1);
rhoC = rho(gmapC); rhouC = rhou(gmapC); rhovC = rhov(gmapC); EnerC = Ener(gmapC);
nxC = gnx(gmapC); nyC = nyin(gmapC);

rhou(gmapC) = -rhou(gmapC); rhov(gmapC) = -rhov(gmapC);
Ener(gmapC) = rhoC*Temp + 0.5*(rhou(gmapC).^2 + rhov(gmapC).^2)./rhoC;
return
