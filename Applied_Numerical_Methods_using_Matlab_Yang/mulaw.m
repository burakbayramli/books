function [y,xmax]=mulaw(x,mu,ymax)
xmax=max(abs(x));
y=ymax*log(1+mu*abs(x/xmax))./log(1+mu).*sign(x);  % Eq.(P1.9a)
