%
% Problem 5.1 in Chapter 5 of Houghton, Carpenter, Collicott 
% and Valentine (2013).
% Prepared by 
% Daniel T. Valentine ......................... March 2012.
clear;clc
% This problem is on an elliptic wing (see Section 5.5.3).
L = 73600;% Lift in Newtons.
s = 15.23; % span in meters.
V = 90; % m/s.
rho = 1.2256*.9762; % kg/m/m/m at altitude = 250 m.
Gamma0 = 4*L/(rho*V*pi*s); % Equation (5.29) pg. 303.
Dv = (pi/8)*rho*Gamma0^2/1000; % Equation (5.33) pg. 305.
G0 = round(Gamma0);
disp([' (a) Induced drag = ' num2str(Dv) ' kN'] )
disp([' (b) Mid-span circulation = ' num2str(G0) ' m*m/s'] )
%
