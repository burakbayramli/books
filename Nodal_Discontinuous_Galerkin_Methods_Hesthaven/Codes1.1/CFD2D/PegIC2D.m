function Q = PegIC2D(x, y, time);
  
%  function [Q] = PegIC2D(x, y, time)
% Purpose: Impose uniform plane flow 

% Example is Mach ** 0.4 ** flow in wind tunnel
gamma = 1.4;

% Inflow conditions -- uniform inflow
rhoin = 1.4; pin = 1.0; Ein = pin/(gamma-1.0);

% pack modified conserved variables
Q(:,:,1) = rhoin*ones(size(x)); 
Q(:,:,2)= 0; Q(:,:,3) = 0; Q(:,:,4) = Ein;
return
