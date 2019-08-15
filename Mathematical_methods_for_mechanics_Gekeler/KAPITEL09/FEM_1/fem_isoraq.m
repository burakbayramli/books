function [ME,BE] = raqell(XK,YK)
% Elliptische RWP, quadratischer Ansatz
% vgl. SCHWARZ: FEM
% liefert die Elementmatrix ME(3,3) und den Elementvektor BE(3)
% fuer ein krummliniges Randelement
% Reihenfolge der Knotenpunkte PA, PM, PB
% INPUT:
%      XK,YK: die drei Koordinatenpaare der Punkte
% OUTPUT:
%      ME, BE

SIG = [0.1127016654;
       0.5;
       0.8872983346];
W   = [0.2777777778;
       0.4444444444;
       0.2777777778];

BE = zeros(3,1); ME = zeros(3,3);
for i = 1:3
   [FF,FFS] = fem_ffquad(SIG(i));
   XFS = XK*FFS; YFS = YK*FFS;
   H = W(i)*sqrt(XFS*XFS + YFS*YFS);
   BE = BE + H*FF;
   ME = ME + H*FF*FF';
end
