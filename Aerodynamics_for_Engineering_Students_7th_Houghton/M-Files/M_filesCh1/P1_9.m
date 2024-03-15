%
% Homewrok problem 1.9: Sailplane
%
clear;clc
b = 18;
AR = 16;
S = b^2/AR;
cm = b/AR;
sig = 0.7;
rhoR = 1.22;
rho = sig*rhoR;
V = 115*1000/3600;
L = 3500;
CL = L/(rho*V^2*S/2)
D = 145;
CD = D/(rho*V^2*S/2)
CM = -0.03;
%
% AMC calculation:
%
dx = .01;
s = b/2;
xf = 0.3;
x = xf:dx:s;
TR = .3;
c_over_co = ( 1 -(1-TR)*(x-xf)/(s-xf) ); 
SS = 2*(sum(c_over_co(2:end-1))+ (c_over_co(1)+c_over_co(end))/2)*dx;
co = S/SS;
c = co*c_over_co;
 c2 = c.^2;
 ca = ( sum(c2(2:end-1))+ (c2(1)+c2(end))/2 ) ...
     /( sum(c(2:end-1)) + (c(1)+c(end))/2 )
 %plot(x/s,c)
 M = CM*(rho*V^2*S/2)*ca
