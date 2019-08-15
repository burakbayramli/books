% Koeffizienten fuer Beispiel lambda_12
format compact
clc
syms pi x
f = sin(pi*x);
g = sin(2*pi*x);
F1 = f*f;
A1 = int(F1,x,0,1);
G1 = g*g;
A2 = int(G1,x,0,1);
a1 = A1*A2;
F2 = f*f*f*f;
B1 = int(F2,x,0,1);
G2 = g*g*g*g;
B2 = int(G2,x,0,1);
b1 = B1*B2;
F3 = f*f*f*g;
C1 = int(F3,x,0,1);
F4 = g*f*f*f;
C2 = int(F4,x,0,1);
c1 = 3*C1*C2;
F4 = f*f*g*g;
D1 = int(F4,x,0,1);
F5 = g*g*f*f;
D2 = int(F5,x,0,1);
d1 = 3*D1*D2;
F6 = f*g*g*g;
E1 = int(F6,x,0,1);
F7 = g*f*f*f;
E2 = int(F7,x,0,1);
e1 = E1*E2;
KOEFF = [a1,b1,c1,d1,e1]
F8 = f*g;
f1 = int(F8,x,0,1)
