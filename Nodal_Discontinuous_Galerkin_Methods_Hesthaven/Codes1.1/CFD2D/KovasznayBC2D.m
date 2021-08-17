function [bcUx, bcUy, bcPR, bcdUndt] = KovasznayBC2D(x, y, nx, ny, mapI, mapO, mapW, mapC, time, nu)

% function [bcUx, bcUy, bcPR, bcdUndt] = KovasznayBC2D(x, y, nx, ny, mapI, mapO, mapW, mapC, time, nu)
% Purpose: evaluate boundary conditions for Kovasznay flow 

zer = zeros(size(x)); bcUx = zer; bcUy = zer; bcPR = zer; bcdUndt = zer;

lam = (0.5/nu) - sqrt( (0.25/(nu^2)) + 4*pi^2 );

% inflow
xI = x(mapI); yI = y(mapI);
bcUx(mapI)= 1-exp(lam*xI).*cos(2*pi*yI);
bcUy(mapI)= (0.5*lam/pi)*exp(lam*xI).*sin(2*pi*yI);

% outflow
xO = x(mapO); yO = y(mapO);

if(0)
  bcPR(mapO) = .5*(1-exp(2*lam*xO));
  bcUx(mapO)= 1-exp(lam*xO).*cos(2*pi*yO);
  bcUy(mapO)= (0.5*lam/pi)*exp(lam*xO).*sin(2*pi*yO);
else
  bcPR(mapO) = .5*(1-exp(2*lam*xO));

  % Neumann data for each velocity
  bcUx(mapO) = -lam*exp(lam*xO).*cos(2*pi*yO);
  bcUy(mapO) = lam*(0.5*lam/pi)*exp(lam*xO).*sin(2*pi*yO);
end
return

