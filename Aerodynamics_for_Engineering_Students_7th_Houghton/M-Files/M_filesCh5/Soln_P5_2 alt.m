%
% Chapter 5 homework solutions via MATLAB
%
% Problem 5.2: Rankine Oval 
clear;clc
m = 120; % m^2/s; 
xso = -1;% m 
yso = 0;
xsi = 1;% m 
ysi = 0;
U = 30; % m/s
c = 1; 
bo = sqrt((m/pi/U)*c + c^2);
to = .5;
for it = 1:1000
to = (m/2/pi/U)*atan2(2*c*to,to^2-c^2); % This converges easily.
end
disp(' ')
disp(' Fineness ratio is equal to bo/to ')
disp('      to       bo       bo/to ')
disp([ to bo bo/to ])
%
% GRAPHICAL SOLUTION:
%
% Karman's line of sources/sinks in uniform stream example
% Rankine oval by D. T. Valentine a draft code written on 
% January 20, 2010 for students of AE/ME425, Spring 2010.
% Applied to solve 3.2 in the text:
%
sig = 120; V=30;
x = -3:.02:3; 
y = -2:.02:2;
for m = 1:length(x)
    for n = 1:length(y)
        xx(m,n) = x(m); yy(m,n) = y(n);
        phis(m,n) = V * x(m) + (sig/4/pi) * log((x(m)+1)^2+y(n)^2)...
            - (sig/4/pi) * log((x(m)-1)^2+ y(n)^2);
        psis(m,n) = V * y(n) + (sig/2/pi) * atan2(y(n),x(m)+1) ...
            - (sig/2/pi) * atan2(y(n),x(m)-1);
    end
end
contour(xx,yy,psis,[0 0],'k'),grid minor
% The fineness ratio is as given in the text. to = 1 and bo = 1.51. 
