function Q = ForwardStepBC2D(xin, yin, nxin, nyin, mapI, mapO, mapW, mapC, Q, time);
  
%  function [Q] = ForwardStepBC2D(xin, yin, nxin, nyin, mapI, mapO, mapW, mapC, Q, time);
% Purpose: Impose channel boundary conditions on 2D Euler equations on weak form

% Example is Mach ** 0.3 ** flow in wind tunnel
gamma = 1.4;

% extract conserved variables
rho = Q(:,:,1); rhou = Q(:,:,2); rhov = Q(:,:,3); Ener = Q(:,:,4);

% Inflow conditions -- uniform inflow
rhoin = gamma; uin = 3.0; vin = 0.0; pin = 1.0;
Ein = pin/(gamma-1.0) + 0.5*rhoin*(uin^2+vin^2);

rho(mapI) = rhoin; rhou(mapI) = rhoin*uin; rhov(mapI) = rhoin*vin; Ener(mapI) = Ein;

% Outflow conditions -- supersonic outflow ( do nothing )

% Wall conditions -- reflective, isothermal, i.e., n.u=0, T=T(t=0)
rhoW = rho(mapW); rhouW = rhou(mapW); rhovW = rhov(mapW); 
nxW = nxin(mapW);   nyW = nyin(mapW);

% reverse flow in normal direction in ghost elements
rhou(mapW) = rhouW - 2*nxW.*(nxW.*rhouW + nyW.*rhovW);
rhov(mapW) = rhovW - 2*nyW.*(nxW.*rhouW + nyW.*rhovW);

% pack modified conserved variables
Q(:,:,1) = rho; Q(:,:,2) = rhou; Q(:,:,3) = rhov; Q(:,:,4) = Ener;
return
