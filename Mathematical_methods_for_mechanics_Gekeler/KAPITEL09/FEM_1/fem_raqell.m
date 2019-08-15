function [ME,BE] = fem_raqell(X,Y)
% Elliptische Probleme, quadratischer Ansatz, vgl. SCHWARZ: FEM
% liefert die Elementmatrix ME(3,3) und den Elementvektor BE(3)
% fuer ein Randelement. Reihenfolge der Punkte: PA, PM PB
% INPUT:  X,Y: die Koordinatenpaare des Anfangs- und des
%         Endpunktes
% OUTPUT: ME, BE

S4  = [4   2  -1; 2  16   2; -1   2   4];
SB  = [1;  4;  1];
X21 = X(2) - X(1); Y21 = Y(2) - Y(1);
L   = sqrt(X21*X21 + Y21*Y21);
ME  = L*S4/30; BE  = L*SB/6;
