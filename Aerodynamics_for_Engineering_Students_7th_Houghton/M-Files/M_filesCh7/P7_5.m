%
% Problem 5.5: Solution of induced drag
% x = y/s, GG = Go/4/s, G = G/Go
clear;clc
syms x G Go s rho U A
w = (Go/4/s)*(11/12+x^2/2)
G = Go*(1 + x^2/6)*sqrt(1-x^2)
intgr = rho*w*G
Do = s*int(intgr,'x',-1,1)
intgL = rho*U*G
L = s*int(intgL,'x',-1,1)
CDo = Do/(rho*U^2* A/2)
CL = L/(rho*U^2* A/2)
f = CDo/CL^2
subs(f,A,'4*s^2/AR')