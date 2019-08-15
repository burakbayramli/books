% Koeffizienten fuer Beispiel lambda_12
% diskreter Fall
format compact
clc
N = 5; MM= 5;
f  = sin(pi*[1:N]/(N+1)); g  = sin(7*pi*[1:N]/(N+1));
h  = sin(5*pi*[1:N]/(N+1));
f  = f'; g = g'; h = h';
u1 = kron(f,g); u2 = kron(g,f); u3 = kron(h,h);
U1 = u1.*u1; U2 = u2.*u2; U3 = u3.*u3;
A1 = u1; A2 = u2; A3 = u3;
B  = U1.*u1;
C  = 3*U1.*u2;
D  = 3*u1.*U2;
E  = u2.*U2;
F  = 3*U1.*u3;
G  = 3*U2.*u3;
H  = 3*u1.*U3;
K  = 3*u2.*U3;
L  = 6*u1.*u2.*u3;
M  = U3.*u3;
KOEFF1 = u1'*[A1,B,C,D,E,F,G,H,K,L,M]/((N+1)*(MM+1));
KOEFF2 = u2'*[A2,B,C,D,E,F,G,H,K,L,M]/((N+1)*(MM+1));
KOEFF3 = u3'*[A3,B,C,D,E,F,G,H,K,L,M]/((N+1)*(MM+1));
A = [KOEFF1;KOEFF2;KOEFF3]
