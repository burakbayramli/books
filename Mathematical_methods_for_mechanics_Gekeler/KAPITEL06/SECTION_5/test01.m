function test01
% auxiliary file for computation of zeros of dV/dx(x,0)
clc
syms x mu
%MU = 0.0122774771;
%MU1 = 1 - MU;
p = x*( (x+mu)^3*((1-mu) - x)^3 - (1-mu)*((1-mu) - x)^3- mu*(x + mu)^3);
q = mu*(1-mu)*((x+mu)^3 - ((1-mu) - x)^3);
P = p + q;
P = simplify(P);
P = simplify(P);

mu =  0.0122774771;
p = x*( (x+mu)^3*((1-mu) - x)^3 - (1-mu)*((1-mu) - x)^3- mu*(x + mu)^3);
q = mu*(1-mu)*((x+mu)^3 - ((1-mu) - x)^3);
P = p + q;
P = simplify(P)
