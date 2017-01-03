function dTdt = rhsSteelHeat(t,T,flag,mc,QV,tcool,htc,As,Ta,emiss)
% rhsSteelHeat  Right hand side of first order ODE for heat treating simulation
%
% Synopsis  dTdt = rhsSteelHeat(t,T,flag,mc,QV,tcool,htc,As,Ta,emiss)
%
% Input:   t     = time (sec)
%          T     = current estimate of bar temperature (K)
%          flag  = (not used) placeholder for compatibility with ode45
%          mc    = product of mass and specific heat capacity (J/K)
%          QV    = rate volumetric heat addition (W)
%          tcool = time (sec) at which heating is stopped and cooling begins
%          htc   = vector of convective heat transfer coefficients (W/m^2/C)
%          As    = surface area of the bar (m^2)
%          Ta    = ambient tempeature (K)
%          emiss = emissivity of the bar (dimensionless)
%
% Output:  dTdt = rate of increase of temperature with time.

if t<tcool
  QVol = QV;  hh = htc(1);    %  heating phase, free convection
else
  QVol = 0;   hh = htc(2);    %  cooling phase, forced convection
end
dTdt = (1/mc)*( QVol - As*( hh*(T - Ta) + emiss*(5.67e-8)*(T^4 - Ta^4) ) );
