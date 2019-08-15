function Z = grad02a(RD,epsilon);
% [Dy-Re], S. 128, Beispiel 1

global n t0 t1 U X Y
A = (t1 - t0)/(2*n); FAKTOR = 2*ones(n+1,1);
FAKTOR(1)   = 1; FAKTOR(n+1) = 1;
W = Y + U./(sqrt(1 + U.*U).*sqrt(1 - X));
Z = RD/epsilon - A*W*FAKTOR;
