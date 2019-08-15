function [SE,ME] = stabelement1(X,Y,E,F);
% liefert die Steifigkeitsmatrix SE(4,4)
% und die Massenmatrix ME(4,4) eines Zugstabes
% in ebener Lage unter Beruecksichtigung
% des Faktors E*F/L, linearer Ansatz
% INPUT:
%        X,Y Koordinatentripel der beiden Eckpunkte
%        E Elastizitaetsmodul,
%        F Querschnittsflaeche des Stabes

C    = zeros(2,1);
X21  = X(2) - X(1);
Y21  = Y(2) - Y(1);
L    = sqrt(X21*X21 + Y21*Y21);
C(1) = X21/L;
C(2) = Y21/L;
H    = C*C';
FAK1 = E*F/L;
FAK2 = L/6;
SE   = FAK1*[  H,   -H;
              -H,    H];
ME   = FAK2*[2*H,    H;
               H,  2*H];
