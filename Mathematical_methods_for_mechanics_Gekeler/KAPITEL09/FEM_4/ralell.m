function [ME,BE] = ralell(X,Y)
% Vgl. SCHWARZ: FEM ---------
% liefert die Elementmatrix ME(2,2) und den Elementvektor BE(2)
% fuer ein Randelement.
% linearer Ansatz, elliptische Probleme
% INPUT:
%      X,Y: die Koordinatenpaare des Anfangs- und des
%           Endpunktes
% OUTPUT:
%      ME, BE
S4  = [2  1; 1  2]; SB  = [1;  1];
X21 = X(2) - X(1);  Y21 = Y(2) - Y(1);
L   = sqrt(X21*X21 + Y21*Y21);
ME  = L*S4/6; BE  = L*SB/2;
