function y = F(uL,uR,dt,dx)

% define the flux
fuL = (uL.^2)/2;
fuR = (uR.^2)/2;

% this is for Lax-Friedrichs
y = (fuL+fuR)/2 - (dx/(2*dt))*(uR-uL);



