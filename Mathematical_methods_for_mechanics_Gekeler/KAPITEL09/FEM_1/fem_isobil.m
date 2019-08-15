function [SE,ME,BE,ecode] = fem_isobil(X,Y)
% Elliptische RWP,  vgl. SCHWARZ:  FEM
% liefert die Elementmatrizen SE(4,4) und ME(4,4), sowie den
% Elementvektor BE(4) fuer ein isoparametrisches Viereckelement
% INPUT:
%      X,Y: die vier Koordinatenpaare der Knotenpunkte in der
%           ueblichen Reihenfolge
%       ecode = 1 bei negativer Jacobi-Determinante
% OUTPUT:
%      SE, ME, BE

ecode = 0;
AUX = 0.5773502692;
SIG = [(1-AUX)/2; (1+AUX)/2];
W   = [0.5; 0.5];

BE = zeros(4,1);
SE = zeros(4,4);
ME = zeros(4,4);
for i1 = 1:2
   for i2 = 1:2
      [FF,FFX,FFE] = fem_ffqbil(SIG(i1),SIG(i2));
      XX = X*FFX;
      XE = X*FFE;
      YX = Y*FFX;
      YE = Y*FFE;
      DET = XX*YE - XE*YX;
      if DET <= 0
         ecode = 1;
      else
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
% ------------------------------------------------
