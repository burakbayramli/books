function y = F(uL,uR,dt,dx)

% define the flux
fuL = (uL.^2)/2;
fuR = (uR.^2)/2;

umid = (uL+uR)/2;
% need df/du at the midpoint:
fp = umid;

% this is for Lax-Wendroff
y = (fuL+fuR)/2 -dt/(2*dx)*fp.*(fuR-fuL);



