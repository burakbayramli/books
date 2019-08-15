% ELSTIF1.M -----------------------------------------
% GEKELER: FINITE ELEMENTE --------------------------
% LIEFERT DIE ELEMENTMATRIZEN SE(9,9) UND ME(9,9) SOWIE DEN
% ELEMENTVEKTOR BE(9) FUER EIN NICHTKONFORMES DREIECKELEMENT
% QUADRATISCHER ANSATZ FUER GRAD W NACH BATOZ ET AL.
% (MOD: INT.J.NME 21, 1985
% PLATTENPROBLEME
% XK, YK : DIE DREI ECKENKOORDINATENPAARE
% E : ELASTIZITAETSMODUL,  NU : POISSONZAHL
% H : DICKE DES ELEMENTES,  P : BELASTUNG DES ELEMENTES
% INPUT:
%      X,Y: die drei Koordinatenpaare der Knotenpunkte in der
%             ueblichen Reihenfolge
%       ecode = 1 bei negativer Jacobi-Determinante
% OUTPUT:
%      SE, BE ME         % BE TO DO

function [SE,ME,BE,ecode] = elstif1(X,Y,E,NU,H,P)
FAKTOR   = E*H*H*H/(12*(1 - NU*NU));
D        = FAKTOR*[1  NU  0;
                   NU 1   0;
                   0  0  (1-NU)/2];
SE       = zeros(9,9);
ME       = zeros(9,9);
BE       = zeros(9,1);
ecode    = 0;
PP       = [12 4 4; 4 2 1; 4 1 2];
GG       = zeros(10,9);
QQ       = zeros(9,9);
Q        = zeros(1,3);
Y23      = Y(2) - Y(3);
Y31      = Y(3) - Y(1);
Y12      = Y(1) - Y(2);
X32      = X(3) - X(2);
X13      = X(1) - X(3);
X21      = X(2) - X(1);

DET      = (Y23*X13 - Y31*X32)*24;
if DET <= 0
   ecode = 1;
end
DD      = [D(1,1)*PP, D(1,2)*PP, D(1,3)*PP;
           D(2,1)*PP, D(2,2)*PP, D(2,3)*PP;
           D(3,1)*PP, D(3,2)*PP, D(3,3)*PP]/DET;
LL1 = Y23*Y23 + X32*X32;
LL2 = Y31*Y31 + X13*X13;
LL3 = Y12*Y12 + X21*X21;
PX  = [6*X32/LL1, 6*X13/LL2,  6*X21/LL3];
PY  = [6*Y23/LL1, 6*Y31/LL2,  6*Y12/LL3];
RX  = [3*X32*X32/LL1, 3*X13*X13/LL2, 3*X21*X21/LL3];
RY  = [3*Y23*Y23/LL1, 3*Y31*Y31/LL2, 3*Y12*Y12/LL3];
Q   = [3*Y23*X32/LL1, 3*Y31*X13/LL2, 3*Y12*X21/LL3];
GG  = [PX(3), -Q(3), -1-RX(3), -PX(3), -Q(3), 1-RX(3), 0, 0, 0;
       -PX(2), -Q(2), -1-RX(2), 0, 0, 0, PX(2), -Q(2), 1-RX(2);
       -PX(3), Q(3), RX(3), PX(3), Q(3), RX(3), 0, 0, 0;
        PX(2)-PX(3), Q(2)+Q(3), RX(2)+RX(3), PX(1)+PX(3),...
        Q(3)-Q(1), RX(3)-RX(1), -PX(1)-PX(2), Q(2)-Q(1), RX(2)-RX(1);
        PX(2), Q(2), RX(2), 0, 0, 0, - PX(2), Q(2), RX(2);
        PY(3), -1-RY(3), -Q(3), - PY(3), 1-RY(3), - Q(3), 0, 0, 0;
       -PY(2), -1-RY(2), - Q(2), 0, 0, 0, PY(2),  1-RY(2), - Q(2);
       -PY(3), RY(3), Q(3), PY(3), RY(3), Q(3), 0, 0, 0;
        PY(2)-PY(3), RY(2)+RY(3), Q(2)+Q(3), PY(1)+PY(3),...
        RY(3)-RY(1), Q(3)-Q(1), -PY(1)-PY(2), RY(2)-RY(1), Q(2)-Q(1);
        PY(2), RY(2), Q(2), 0, 0, 0, -PY(2), RY(2), Q(2)];

QQ = [Y31*GG(1,:)   + Y12*GG(2,:);
      2*Y31*GG(3,:) + Y12*GG(4,:);
      Y31*GG(4,:)   + 2*Y12*GG(5,:);
    - X13*GG(6,:)   - X21*GG(7,:);
    - 2*X13*GG(8,:) - X21*GG(9,:);
    - X13*GG(9,:) - 2*X21*GG(10,:);
      X13*GG(1,:) + X21*GG(2,:) - Y31*GG(6,:) - Y12*GG(7,:);
    2*X13*GG(3,:) + X21*GG(4,:) - 2*Y31*GG(8,:) - Y12*GG(9,:);
      X13*GG(4,:) + 2*X21*GG(5,:) - Y31*GG(9,:) - 2*Y12*GG(10,:)];
SE = QQ'*DD*QQ;
BE = [P, 0, 0, P, 0, 0, P, 0, 0]'*DET/144;
