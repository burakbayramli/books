function test09
% Polarkoordinaten, Metriktensor und Christoffelsymbole
clc
% u = r, v = phi
syms u v x y
X =  [u*cos(v); u*sin(v)];
G1 = diff(X,'u');
G2 = diff(X,'v');
phi = acos(x/sqrt(x*x + y*y));
phidx = diff(phi,'x');
phidx = simplify(phidx);

