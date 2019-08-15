function [ME,BE] = rakell(X,Y)
% Elliptische RWP, vgl. SCHWARZ: FEM
% liefert die Elementmatrix ME(6,6) und den Elementvektor BE(6)
% fuer ein Randelement. kubisch-hermitescher Ansatz
% INPUT:
%      X,Y: die Koordinatenpaare des Anfangs- und des
%             Endpunktes
% OUTPUT:
%      ME, BE

S4 = [156 22 22  54   -13 -13;
       22  4  4  13    -3  -3;
       22  4  4  13    -3  -3;
       54 13 13 156   -22 -22;
      -13 -3 -3 -22     4   4;
      -13 -3 -3 -22     4   4];
SB  = [6;  1;  1; 6; -1; -1];
X21 = X(2) - X(1); Y21 = Y(2) - Y(1);
L   = sqrt(X21*X21 + Y21*Y21);
BE  = L*SB/12; ME  = L*S4/420;
for I = [2,5]
   BE(I)   = X21*BE(I);
   BE(I+1) = Y21*BE(I+1);
   for J = 1:6
      ME(I,J)   = X21*ME(I,J);
      ME(I+1,J) = Y21*ME(I+1,J);
      ME(J,I)   = X21*ME(J,I);
      ME(J,I+1) = Y21*ME(J,I+1);
   end
end
