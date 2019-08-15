function [SE,ME,BE,ecode] = fem_batoz(XK,YK,E,NU,H,P)
% GEKELER: FINITE ELEMENTE --------------------------
% LIEFERT DIE ELEMENTMATRIZEN SE(9,9) UND ME(9,9) SOWIE DEN
% ELEMENTVEKTOR BE(9) FUER EIN NICHTKONFORMES DREIECKELEMENT
% QUADRATISCHER ANSATZ FUER GRAD W NACH BATOZ ET AL.
% PLATTENPROBLEME
% XK, YK : DIE DREI ECKENKOORDINATENPAARE
% E : ELASTIZITAETSMODUL,  NU : POISSONZAHL
% H : DICKE DES ELEMENTES,  P : BELASTUNG DES ELEMENTES
% INPUT:
%      XK,YK: die drei Koordinatenpaare der Knotenpunkte in der
%             ueblichen Reihenfolge
%       ecode = 1 bei negativer Jacobi-Determinante
% OUTPUT:
%      SE, BE, CE, ME

FAKTOR = E*H*H*H/(12*(1 - NU*NU));
SE = sparse(9,9); ME = sparse(9,9); BE = zeros(9,1);
ecode = 0;

XSI = [0.333333333; 0.470142064; 0.059715872;
       0.470142064; 0.101286507; 0.797426985;
       0.101286507];
ETA = [0.333333333; 0.470142064; 0.470142064;
       0.059715872; 0.101286507; 0.101286507;
       0.797426985];
W   = [0.1125;
       0.0661970764;
       0.0661970764;
       0.0661970764;
       0.0629695903;
       0.0629695903;
       0.0629695903];
D = [1  NU  0; NU  1  0; 0  0  (1 - NU)/2];
for I = 1:7
   [B,DET,ecode] = fem_batoz1(XK,YK,XSI(I),ETA(I));
   SE = SE + B*D*B'*W(I);
end
SE = FAKTOR*SE/DET;
BE = [P, 0, 0, P, 0, 0, P, 0, 0]'*DET/6;
