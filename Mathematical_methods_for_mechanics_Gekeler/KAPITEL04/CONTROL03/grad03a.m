function Z = grad03a(name,H0_u,H1_u,epsilon);
% Berechnung von Z im Gradientenverfahren

global n t1 U X
A           = t1/(2*n);
FAKTOR      = 2*ones(n+1,1);
FAKTOR(1)   = 1;
FAKTOR(n+1) = 1;
H1          = H1_u(1,:);
H2          = H1_u(2,:);
AUX1        = H1.*H0_u;
AUX2        = H2.*H0_u;
R           = zeros(2,1);
R(1)        = A*AUX1*FAKTOR;
R(2)        = A*AUX2*FAKTOR;
AUX1        = H1.*H1;
AUX2        = H1.*H2;
AUX3        = H2.*H2;
Q           = zeros(2,2);
Q(1,1)      = A*AUX1*FAKTOR;
Q(1,2)      = A*AUX2*FAKTOR;
Q(2,2)      = A*AUX3*FAKTOR;
Q(2,1)      = Q(1,2);
DK          = feval(name,4,0,0,0);
R           = DK/epsilon - R;
Z           = Q\R;
% Nachiteration ---------------
for I = 1:3
   R1 = R - Q*Z;
   v = Q\R1;
   Z = (Z + v);
end
Z = Z';
%KOND_Q = cond(Q)
