function [SE,ME] = balken2(X,Y,H,B,PARMETER);
% wie BALKEN.M aber an MATLAB angepasst ----------------------
% liefert die Steifigkeitsmatrix SE(6,6) und die Massenmatrix
% ME(6,6) eines Balkenelementes in ALLGEMEINER ebener Lage ohne Torsion
% INPUT:
%       X,Y Koordinaten der beiden Eckpunkte
%       H Hoehe des rechteckigen Querschnittes
%       B Breite des rechteckigen Querschnittes
%       E Elastizitaetsmodul
%       NU POISSONzahl
%       RHO spezifisches Gewicht
% OUTPUT:
%       SE, ME

E   = PARMETER(1);
NU  = PARMETER(2);
RHO = PARMETER(3);
X21     = X(2) - X(1);
Y21     = Y(2) - Y(1);
L       = sqrt(X21*X21 + Y21*Y21);
SL = [  1  -1;
       -1   1];
ML = [  2   1;
        1   2];
SK = [  6     3*L     -6     3*L;
        3*L   2*L*L   -3*L   1*L*L;
       -6    -3*L      6    -3*L;
        3*L   1*L*L   -3*L   2*L*L];
MK = [156    22*L     54    -13*L;
       22*L   4*L*L   13*L   -3*L*L;
       54    13*L    156    -22*L;
      -13*L  -3*L*L  -22*L    4*L*L];
C1 = E*B*H*H*H/(6*L*L*L);
C3 = E*B*H/L;
C5 = RHO*B*H*L/420;
C6 = RHO*B*H*L/6;
N2 = zeros(4,2);
N3 = zeros(2,2);
SE = [C1*SK,    N2;
        N2', C3*SL];
ME = [C5*MK,    N2;
        N2', C6*ML];

c = X21/L;
s = Y21/L;
C1      = [ c, s, 0;
           -s, c, 0;
            0, 0, c];
C2 = [0, 0, 0;
      0, 0, 0;
      0, 0, s];
D       = [ C1,  C2; -C2, C1];
P       = [5, 1, 2, 6, 3, 4];
ME      = ME(P,P);
SE      = SE(P,P);
ME      = D'*ME*D;
SE      = D'*SE*D;
