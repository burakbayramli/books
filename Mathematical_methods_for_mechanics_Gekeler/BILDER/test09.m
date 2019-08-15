% Triformen fuer lineare Dreieckselemente
clc
clear
syms u1 u2 u3 v1 v2 v3 w1 w2 w3
A = [6, 2, 2; 2, 2, 1; 2, 1, 2];
B = [2, 2, 1; 2, 6, 2; 1, 2, 2];
C = [2, 1, 2; 1, 2, 2; 2, 2, 6];

u = [u1; u2; u3];
v = [v1, v2, v3];
w = [w1; w2; w3];

F = u1*A + u2*B + u3*C;
Z1 = v*F*w;

v = [v1;v2; v3];
u = [u1, u2, u3];
P1 = [6, 2, 2; 2, 2, 1; 2, 1, 2];
P2 = [2, 2, 1; 2, 6, 2; 1, 2, 2];
P3 = [2, 1, 2; 1, 2, 2; 2, 2, 6];
G1 = P1*v; G2 = P2*v; G3 = P3*v;
G = [G1, G2, G3];
Z2= u*G*w;
DIFF = Z1 - Z2;
DIFF = simplify(DIFF);
DIFF = simplify(DIFF)

