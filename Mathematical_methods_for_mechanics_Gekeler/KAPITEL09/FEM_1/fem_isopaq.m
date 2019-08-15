function [SE,ME,BE,ecode] = isopaq(XK,YK)
% Elliptische Probleme, vgl. SCHWARZ: FEM
% quadratischer Ansatz der Serentipity-Klasse
% liefert die Elementmatrizen SE(8,8) und ME(8,8), sowie den
% Elementvektor BE(8) fuer ein isoparametrisches Viereckelement
% INPUT:
%      XK,YK: die acht Koordinatenpaare der Knotenpunkte in der
%             ueblichen Reihenfolge
%       ecode = 1 bei negativer Jacobi-Determinante
% OUTPUT:
%      SE, ME, BE

ecode = 0;
SIG = [0.1127016654;
       0.5;
       0.8872983346];
W   = [0.2777777778;
       0.4444444444;
       0.2777777778];

BE = zeros(8,1); SE = zeros(8,8); ME = zeros(8,8);
for i1 = 1:3
   for i2 = 1:3
      [FF,FFX,FFE] = fem_ffqpas(SIG(i1),SIG(i2));
      XX = XK*FFX;  XE = XK*FFE;
      YX = YK*FFX;  YE = YK*FFE;
      DET = XX*YE - XE*YX;
      if DET <= 0, ecode = 1; else
         WDET = sqrt(DET);
         H1 = (YE*FFX - YX*FFE)/WDET;
         H2 = (XX*FFE - XE*FFX)/WDET;
         WW = W(i1)*W(i2);
         H  = WW*DET;
         BE = BE +  H*FF;
         SE = SE + WW*(H1*H1' + H2*H2');
         ME = ME + H*FF*FF';
      end
   end
end
