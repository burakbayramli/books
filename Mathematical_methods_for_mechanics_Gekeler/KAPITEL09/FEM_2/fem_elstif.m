% ELSTIF.M -----------------------------------------
% GEKELER: FINITE ELEMENTE -------------------------
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
%      SE, BE, CE, ME

function [SE,ME,BE,ecode] = elstif(X,Y,E,NU,H,P)
FAKTOR   = E*H*H*H/(12*(1 - NU*NU));
D        = FAKTOR*[1  NU  0;
                   NU 1   0;
                   0  0  (1-NU)/2];
SE       = zeros(9,9);
ME       = zeros(9,9);
BE       = zeros(9,1);
ecode    = 0;
KOD      = [1 2 3 4 5 6 7 8 9;
            1 3 2 4 6 5 7 9 8];
PP       = [12 4 4; 4 2 1; 4 1 2];
B        = zeros(3,1);
C        = zeros(3,1);
PT       = zeros(2,3);
RS       = zeros(2,3);
GG       = zeros(10,9);
QQ       = zeros(9,9);
Q        = zeros(1,3);
B(1)     = Y(2) - Y(3);
B(2)     = Y(3) - Y(1);
B(3)     = Y(1) - Y(2);
C(1)     = X(3) - X(2);
C(2)     = X(1) - X(3);
C(3)     = X(2) - X(1);

DET      = (B(1)*C(2) - B(2)*C(1))*24;
if DET <= 0
   ecode = 1;
end
DD      = [D(1,1)*PP, D(1,2)*PP, D(1,3)*PP;
           D(2,1)*PP, D(2,2)*PP, D(2,3)*PP;
           D(3,1)*PP, D(3,2)*PP, D(3,3)*PP]/DET;
ALS     = B.*B + C.*C;
PT(1,:) = (6*C./ALS)';
PT(2,:) = (6*B./ALS)';
RS(1,:) = (3*C.*C./ALS)';
RS(2,:) = (3*B.*B./ALS)';
Q       = 3*B.*C./ALS;
for I = 1:2
   II      = (I-1)*5;
   GG(II+1,KOD(I,1)) =   PT(I,3);
   GG(II+2,KOD(I,1)) = - PT(I,2);
   GG(II+3,KOD(I,1)) = - PT(I,3);
   GG(II+4,KOD(I,1)) =   PT(I,2) - PT(I,3);
   GG(II+5,KOD(I,1)) =   PT(I,2);

   GG(II+1,KOD(I,2)) = - Q(3);
   GG(II+2,KOD(I,2)) = - Q(2);
   GG(II+3,KOD(I,2)) =   Q(3);
   GG(II+4,KOD(I,2)) =   Q(2) + Q(3);
   GG(II+5,KOD(I,2)) =   Q(2);

   GG(II+1,KOD(I,3)) = - 1 - RS(I,3);
   GG(II+2,KOD(I,3)) = - 1 - RS(I,2);
   GG(II+3,KOD(I,3)) =   RS(I,3);
   GG(II+4,KOD(I,3)) =   RS(I,2) + RS(I,3);
   GG(II+5,KOD(I,3)) =   RS(I,2);

   GG(II+1,KOD(I,4)) = - PT(I,3);
   GG(II+3,KOD(I,4)) =   PT(I,3);
   GG(II+4,KOD(I,4)) =   PT(I,1) + PT(I,3);

   GG(II+1,KOD(I,5)) = - Q(3);
   GG(II+3,KOD(I,5)) =   Q(3);
   GG(II+4,KOD(I,5)) =   Q(3) - Q(1);

   GG(II+1,KOD(I,6)) =   1 - RS(I,3);
   GG(II+3,KOD(I,6)) =   RS(I,3);
   GG(II+4,KOD(I,6)) =   RS(I,3) - RS(I,1);

   GG(II+2,KOD(I,7)) =   PT(I,2);
   GG(II+4,KOD(I,7)) = - PT(I,1) - PT(I,2);
   GG(II+5,KOD(I,7)) = - PT(I,2);

   GG(II+2,KOD(I,8)) = - Q(2);
   GG(II+4,KOD(I,8)) =   Q(2) - Q(1);
   GG(II+5,KOD(I,8)) =   Q(2);

   GG(II+2,KOD(I,9)) =   1 - RS(I,2);
   GG(II+4,KOD(I,9)) =   RS(I,2) - RS(I,1);
   GG(II+5,KOD(I,9)) =   RS(I,2);
end

QQ(1,:) =   B(2)*GG(1,:)   + B(3)*GG(2,:);
QQ(2,:) =   2*B(2)*GG(3,:) + B(3)*GG(4,:);
QQ(3,:) =   B(2)*GG(4,:)   + 2*B(3)*GG(5,:);
QQ(4,:) = - C(2)*GG(6,:)   - C(3)*GG(7,:);
QQ(5,:) = - 2*C(2)*GG(8,:) - C(3)*GG(9,:);
QQ(6,:) = - C(2)*GG(9,:) - 2*C(3)*GG(10,:);
QQ(7,:) =   C(2)*GG(1,:) + C(3)*GG(2,:) - B(2)*GG(6,:)...
            - B(3)*GG(7,:);
QQ(8,:) =   2*C(2)*GG(3,:) + C(3)*GG(4,:) - 2*B(2)*GG(8,:)...
            - B(3)*GG(9,:);
QQ(9,:) =   C(2)*GG(4,:) + 2*C(3)*GG(5,:) - B(2)*GG(9,:)...
            - 2*B(3)*GG(10,:);
SE      = QQ'*DD*QQ;
BE      = [P, 0, 0, P, 0, 0, P, 0, 0]'*DET/144;
