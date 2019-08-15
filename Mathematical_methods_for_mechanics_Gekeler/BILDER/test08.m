% Triformen fuer lineare Dreieckselemente
clc
syms xi eta
f1 = (1-xi-eta)*(1-xi-eta)*[1-xi-eta, xi, eta];
g1 = int(f1,xi,0,1-eta);
h1 = int(g1,eta,0,1);
H1 = simplify(h1);
%
f1 = (1 - xi- eta)*xi*[1-xi-eta, xi, eta];
g1 = int(f1,xi,0,1-eta);
h1 = int(g1,eta,0,1);
H2 = simplify(h1);
%
f1 = (1-xi-eta)*eta*[1-xi-eta, xi, eta];
g1 = int(f1,xi,0,1-eta);
h1 = int(g1,eta,0,1);
H3 = simplify(h1);
P1 = 120*[H1;H2;H3]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
f1 = xi*(1-xi-eta)*[1-xi-eta, xi, eta];
g1 = int(f1,xi,0,1-eta);
h1 = int(g1,eta,0,1);
H1 = simplify(h1);
%
f1 = xi*xi*[1-xi-eta, xi, eta];
g1 = int(f1,xi,0,1-eta);
h1 = int(g1,eta,0,1);
H2 = simplify(h1);
%
f1 = xi*eta*[1-xi-eta, xi, eta];
g1 = int(f1,xi,0,1-eta);
h1 = int(g1,eta,0,1);
H3 = simplify(h1);
P2 = 120*[H1;H2;H3]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
f1 = eta*(1-xi-eta)*[1-xi-eta, xi, eta];
g1 = int(f1,xi,0,1-eta);
h1 = int(g1,eta,0,1);
H1 = simplify(h1);
%
f1 = eta*xi*[1-xi-eta, xi, eta];
g1 = int(f1,xi,0,1-eta);
h1 = int(g1,eta,0,1);
H2 = simplify(h1);
%
f1 = eta*eta*[1-xi-eta, xi, eta];
g1 = int(f1,xi,0,1-eta);
h1 = int(g1,eta,0,1);
H3 = simplify(h1);
P3 = 120*[H1;H2;H3]

