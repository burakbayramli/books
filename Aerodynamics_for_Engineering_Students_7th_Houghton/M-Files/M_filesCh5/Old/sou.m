%
%   Karman's line of sources/sinks in uniform stream example: The 
% Rankine oval by D. T. Valentine a draft code written Spring 2010
% reviewed in the Summer of 2016.
%
clear;clc
sig = 2; V=1;
x = -2:.02:2; 
y = -2:.02:2;
for m = 1:length(x)
    for n = 1:length(y)
        xx(m,n) = x(m); yy(m,n) = y(n);
        phis(m,n) = V * x(m) + (sig/4/pi) * log(x(m)^2+(y(n)+.01)^2)...
            - (sig/4/pi) * log((x(m)-.501)^2+(y(n)+.01)^2);
        psis(m,n) = V * y(n) + (sig/2/pi) * atan2(y(n),x(m)) ...
            - (sig/2/pi) * atan2(y(n),(x(m)-.501));
    end
end
contour(xx,yy,psis),hold on,contour(xx,yy,phis)