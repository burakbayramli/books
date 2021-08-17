function Q = ChannelIC2D(x, y, time);
  
%  function [Q] = ChannelIC2D(x, y, time)
%  Purpose: Impose uniform plane flow 

% Example is Mach ** 0.4 ** flow in wind tunnel
gamma = 1.4;

% Inflow conditions -- uniform inflow
rhoin = 1.4; uin = 0.4; vin = 0.0; pin = 1.0;
Ein = pin/(gamma-1.0) + 0.5*rhoin*(uin^2+vin^2);

% pack modified conserved variables

Q(:,:,1) = rhoin*ones(size(x)); 
Q(:,:,2)= rhoin*(1/.41)^2*6*(y+.2).*(0.41 - (y+.2));
Q(:,:,3) = 0;
Q(:,:,4) = Ein + 0.5*(Q(:,:,2).^2 + Q(:,:,3).^2)./Q(:,:,1);
return
