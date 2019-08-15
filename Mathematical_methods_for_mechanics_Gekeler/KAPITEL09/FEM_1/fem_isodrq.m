function [SE,ME,BE,ecode] = fem_isodrq(XK,YK)
% Elliptische RWP, quadratischer Ansatz
% vgl. SCHWARZ: FEM
% liefert die Elementmatrizen SE(6,6) und ME(6,6) sowie
% den Elementvektor BE(6) fuer ein isoparametrisches
% Dreieckelement
% INPUT:
%        XK,YK die sechs Koordinatenpaare in der
%        ueblichen Reihenfolge
%        ecode = 1 bei negativer Jacobi-Determinante

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
ecode = 0;
SE = zeros(6,6); ME = zeros(6,6); BE = zeros(6,1);
for i = 1:7
   A = XSI(i);  B = ETA(i);
   [FF,FFX,FFE] = fem_ffqdre(A,B);
   XX = XK*FFX;  XE = XK*FFE;
   YX = YK*FFX;  YE = YK*FFE;
   DET = XX*YE - XE*YX;
   if DET <= 0, ecode = 1;    else
      WDET = sqrt(DET);
      H1 = (YE*FFX - YX*FFE)/WDET;
      H2 = (XX*FFE - XE*FFX)/WDET;
      H  = W(i)*DET;
      BE =   BE + H*FF;
      SE = SE + W(i)*(H1*H1' + H2*H2');
      ME = ME + H*FF*FF';
   end
end
