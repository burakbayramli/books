function [Ux, Uy, PR] = KovasznayIC2D(x, y, time, nu)

% function [Ux, Uy, PR] = KovasznayIC2D(x, y, time, nu)
% Purpose: evaluate solution for Kovasznay flow 

lam = (0.5/nu) - sqrt( (0.25/(nu^2)) + 4*pi^2 );

Ux= 1-exp(lam*x).*cos(2*pi*y); Uy= (0.5*lam/pi)*exp(lam*x).*sin(2*pi*y);

PR = .5*(1-exp(2*lam*x));
return



