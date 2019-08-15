% Koeffizienten fuer Beispiel lambda_12
% diskreter Fall
format compact
clc
%syms x
N  = 5; M = 5;
f  = sin(pi*[1:N]/(N+1)); g = sin(2*pi*[1:N]/(N+1));
f  = f'; g = g';
u1 = kron(f,g); u2 = kron(g,f);
U1 = u1.*u1; U2 = u2.*u2;
A1 = u1;
B  = U1.*u1;
C  = 3*(U1.*u2);
D  = 3*(u1.*U2);
E  = (u2.*U2);
KOEFF1 = u1'*[A1,B,C,D,E]/((N+1)*(M+1))
A2     = u2;
KOEFF2 = u2'*[A2,B,C,D,E]/((N+1)*(M+1))
