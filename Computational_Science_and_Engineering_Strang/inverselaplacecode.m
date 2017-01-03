%5.3  inverselaplacecode.m

function f = inverselaplace(F,t,N)  % Find f(t) by numerical inversion, t > 0
a = -1.2244*N/t; b = 1.0035*N/t; c = 0.5272; d = 0.6407; % tuned parameters
theta = (2*[0:N-1]+1)*pi/(2*N);                          % equispaced angles
s = a + b*(theta.*cot(d*theta) + c*i*theta);             % modified contour
D = b*(cot(d*theta)-d*theta.*csc(d*theta).^2+c*i);       % D = ds/dtheta
f = imag(sum(exp(s*t).*F(s).*D))/N;                      % f(t) = midpoint sum
% example F = @(s) exp(-sqrt(s)); f = inverselaplace(F,1,8) in Problem 30
