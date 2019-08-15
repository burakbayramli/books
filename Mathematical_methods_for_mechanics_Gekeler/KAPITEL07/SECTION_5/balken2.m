function [SE,ME] = balken2(X,Y,Z,H,B,PARMETER);
% Vgl. SCHWARZ: FEM ----------------
% liefert die Steifigkeitsmatrix SE(12,12) und die Massenmatrix
% ME(12,12) eines Balkenelementes in fast allgemeiner Lage:
% Es gilt die Einschraenkung, dass die Breitseite
% der Querschnittsflaeche senkrecht zur z-Achse bleibt.
% INPUT:
%       X,Y,Z Koordinatentripel der beiden Eckpunkte
%       H Hoehe des rechteckigen Querschnittes
%       B Breite des rechteckigen Querschnittes
%       E Elastizitaetsmodul
%       NU POISSONzahl
%       RHO spezifisches Gewicht

E   = PARMETER(1);
NU  = PARMETER(2);
RHO = PARMETER(3);
X21 = X(2) - X(1);
Y21 = Y(2) - Y(1);
Z21 = Z(2) - Z(1);
L   = sqrt(X21*X21 + Y21*Y21 + Z21*Z21);
L1  = sqrt(X21*X21 + Y21*Y21)/L;
SL = [  1  -1;
       -1   1];
ML = [  2   1;
        1   2];
SK = [  6     3*L     -6     3*L;
        3*L   2*L*L   -3*L   1*L*L;
       -6    -3*L      6    -3*L;
        3*L   1*L*L   -3*L   2*L*L];
SK1= [  6    -3*L     -6    -3*L;
       -3*L   2*L*L    3*L   1*L*L;
       -6     3*L      6     3*L;
       -3*L   1*L*L    3*L   2*L*L];
MK = [156    22*L     54    -13*L;
       22*L   4*L*L   13*L   -3*L*L;
       54    13*L    156    -22*L;
      -13*L  -3*L*L  -22*L    4*L*L];
MK1= [156   -22*L     54     13*L;
      -22*L   4*L*L  -13*L   -3*L*L;
       54   -13*L    156     22*L;
       13*L  -3*L*L   22*L    4*L*L];
Q       = H/B;
B1      = B;
if Q < 1
   Q    = B/H;
   B1   = H;
end
ETA2 = ((2.370592*Q - 2.486211)*Q + 0.826518)/...
       ((7.111777*Q - 3.057824)*Q + 1);
IT   = ETA2*B*H*B1*B1;
IP   = B*H*(B*B + H*H)/12;
C1   = E*B*H*H*H/(6*L*L*L);
C2   = E*H*B*B*B/(6*L*L*L);
C3   = E*B*H/L;
C4   = E*IT/(2*L*(1 + NU));
C5   = RHO*B*H*L/420;
C6   = RHO*B*H*L/6;
C7   = RHO*IP*L/6;
N1   = zeros(4,4);
N2   = zeros(4,2);
N3   = zeros(2,2);
SE = [C1*SK,    N1,    N2,    N2;
         N1, C2*SK1,   N2,    N2;
        N2',   N2', C3*SL,    N3;
        N2',   N2',    N3, C4*SL];
ME = [C5*MK,     N1,    N2,    N2;
         N1, C5*MK1,    N2,    N2;
        N2',    N2', C6*ML,    N3;
        N2',    N2',    N3, C7*ML];
C       = zeros(3,3);
C(1,:)  = [X21, Y21, Z21]/L;
C(2,:)  = [   0,   1,   0];
if abs(L1) > 1.0E-10
   C(2,1:2) = [-C(1,2), C(1,1)]/L1;
end
C(3,:)  = [-C(1,3)*C(2,2), C(1,3)*C(2,1),...
                             C(1,1)*C(2,2)-C(1,2)*C(2,1)];
N3   = zeros(3,3);
D    = [ C,  N3, N3, N3;
        N3,   C, N3, N3;
        N3,  N3,  C, N3;
        N3,  N3, N3,  C];
P    = [9, 5, 1, 11, 2, 6, 10, 7, 3, 12, 4, 8];
ME   = ME(P,P);
SE   = SE(P,P);
ME   = D'*ME*D;
SE   = D'*SE*D;
