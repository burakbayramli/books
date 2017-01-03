function y = g(x)

% discontinuous + periodic
% y = sign(sin(x));

% jump in first derivative:
% y = abs(sin(x));

% jump in second derivative:
y = (cos(2*x)-1).*sign(mod(x/pi,2)-1);

% beautiful function:
% y = cos(2*x);
