% Koeffizienten fuer Beispiel lambda_17
format compact
clc
syms pi x
f = sin(pi*x); g = sin(7*pi*x); h = sin(5*pi*x);
F1 = f*f;
A1 = int(F1,x,0,1);
G1 = g*g;
A2 = int(G1,x,0,1);
a1 = A1*A2;
% -- B ----
F2 = f*f*f*f;
B1 = int(F2,x,0,1);
G2 = g*g*g*g;
B2 = int(G2,x,0,1);
b1 = B1*B2;
% -- C -----
F3 = f*f*f*g;
C1 = int(F3,x,0,1);
G3 = g*f*f*f;
C2 = int(G3,x,0,1);
c1 = 3*C1*C2;
% -- D -----
F4 = f*f*g*g;
D1 = int(F4,x,0,1);
d1 = 3*D1*D1;
% -- E -------
F5 = f*g*g*g;
E1 = int(F5,x,0,1);
G5 = f*f*f*g;
E2 = int(G5,x,0,1);
e1 = E1*E2;
% -- F -------
F6 = f*f*f*h;
FF1 = int(F6,x,0,1);
G6 = g*g*g*h;
FF2 = int(G6,x,0,1);
f1 = 3*FF1*FF2;
% -- G -------
F7 = f*f*h*h;
G1 = int(F7,x,0,1);
G7 = g*g*h*h;
G2 = int(G7,x,0,1);
g1 = 3*G1*G2;
% -- H ------
F8 = f*g*g*h;
H1 = int(F8,x,0,1);
G8 = f*f*g*h;
H2 = int(G8,x,0,1);
h1 = 3*H1*H2;
% -- K ------
F9 = f*g*h*h;
K1 = int(F9,x,0,1);
k1 = 3*K1*K1;
% -- L -----
F10 = f*f*g*h;
L1 = int(F10,x,0,1);
G10 = f*g*g*h;
L2 = int(G10,x,0,1);
l1 = 6*L1*L2;
% -- M -------
F11 = f*h*h*h;
M1 = int(F11,x,0,1);
G11 = g*h*h*h;
M2 = int(G11,x,0,1);
m1 = M1*M2;
KOEFFA = [a1,b1,c1,d1,e1,f1]
KOEFFB = [g1,h1,k1,l1,m1]
F12 = h*h*h*h;
N1 = int(F12,x,0,1);
m3 = N1*N1
