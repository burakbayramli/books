function [SE,ME] = stabelement2(X,Y,Z,E,F);
% GEKELER: FINITE ELEMENTE, vgl. SCHWARZ: FEM --------------
% liefert die Steifigkeitsmatrix SE(6,6) und die
% Massenmatrix ME(6,6) eines Zugstabes
% in allgemeiner raeumlicher Lage unter Beruecksichtigung
% des Faktors E*A/L, linearer Ansatz
% INPUT:
%        X,Y,Z Koordinatentripel der beiden Eckpunkte
%        E Elastizitaetsmodul, F Querschnittsflaeche des Stabes

C    = zeros(3,1);
X21  = X(2) - X(1);
Y21  = Y(2) - Y(1);
Z21  = Z(2) - Z(1);
L    = sqrt(X21*X21 + Y21*Y21 + Z21*Z21);
C(1) = X21/L;
C(2) = Y21/L;
C(3) = Z21/L;
H    = C*C';
FAK1 = E*F/L;
FAK2 = L/6;
SE   = FAK1*[  H,   -H;
              -H,    H];
ME   = FAK2*[2*H,    H;
               H,  2*H];
