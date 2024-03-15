%
%   Chapter 5 Problem 5.2 solution via MATLAB
% by Daniel T. Valentine, Spring 2012.
%
% The problem asks for the length diivided by the width
% ratio for a Rankine oval in a uniform stream. This 
% problem is described in Section 3.3.7 (pp 125-126) in the 
% text. 
clear;clc
% Given information (input):
m = 120;% m^2/s; 
xso = -1;% m 
yso = 0;
xsi = 1;% m 
ysi = 0;
U = 30; % m/s
c = 1;% half-distance between source and sink.
%
% SOLUTION METHOD 1: Substitution into formulas for bo and to.
% Equation (5.36)  is the formula for the half chord. 
% It is
bo = sqrt((m/pi/U)*c + c^2);
% Equation (5.35) is a formula for the half 
% thickness it can be solved interatively by successive 
% approximation as follows: 
to = .5; % Initial guess
for it = 1:1000 % 1000 iterations
to = (m/2/pi/U)*atan2(2*c*to,to^2-c^2); % This converges easily.
end
% Results: Output to command window
disp(' ')
disp(' Fineness ratio is equal to bo/to ')
disp('      to       bo       bo/to ')
disp([ to bo bo/to ])
% RESULTS PRINTED TO COMMAND WINDOW
% Fineness ratio is equal to bo/to 
%       to       bo       bo/to 
%    1.0000    1.5077    1.5077
%
% SOLUTION METHOD 2: GRAPHICAL SOLUTION that applies the 
% formulas in Section 5.3.7 to find phis (velocity potential) 
% and psis (stream function) for the Rankine oval applied to 
% solve 5.2 in the text:
x = -3:.02:3; 
y = -2:.02:2;
for mm = 1:length(x)
    for nn = 1:length(y)
        xx(mm,nn) = x(mm); yy(mm,nn) = y(nn);
        phis(mm,nn) = U * x(mm) ...
            + (m/4/pi) * log((x(mm)-xso)^2+y(nn)^2)...
            - (m/4/pi) * log((x(mm)-xsi)^2+ y(nn)^2);
        psis(mm,nn) = U * y(nn) ...
            + (m/2/pi) * atan2(y(nn),x(mm)-xso) ...
            - (m/2/pi) * atan2(y(nn),x(mm)-xsi);
        psi2(mm,nn) = U * y(nn) ...
            + (m/2/pi)*atan2(-2*y(nn)*c,x(mm)^2+y(nn)^2-c^2);
    end
end
contour(xx,yy,psis,[0 0],'k'),grid minor
title('Illustration of Rankine oval')
xlabel('x'),ylabel('y')
hold on
contour(xx,yy,psis,20)
contour(xx,yy,phis)
legend('oval','\psi', '\phi')
hold off
figure(2)
contour(xx,yy,psi2)
% Remark: On the graph of the oval to = 1 and bo = 1.51. Hence, 
% the graph of the surface of the oval checks with the fineness 
% ratio given in the text and computed by method 1.
