function [SIGD,SIGX,SIGY,TAUXY,SIG1,SIG2,PHI,EV1,EV2]...
             = spaqua1(WERTE,E,NU)
% berechnet die Spannungen in den Knoten
% kubische Elemente
% INPUT:
%        WERTE = (U,V,U_X,V_X,U_Y,V_Y)'
%        E Elastizitaetsmodul
%        NU POISSONzahl

EPSX  = WERTE(3); EPSY  = WERTE(6); GAMXY = WERTE(5) + WERTE(4);
SIGX  = E*(EPSX + NU*EPSY)/(1 - NU*NU);
SIGY  = E*(NU*EPSX + EPSY)/(1 - NU*NU);
TAUXY = E*GAMXY/(2*(1 + NU));
DIF   = SIGY - SIGX;
SIGD  = sqrt(DIF*DIF + 4*TAUXY*TAUXY);
SIG1  = (SIGX + SIGY + SIGD)/2;
SIG2  = SIG1 - SIGD;
PHI   = atan(2*TAUXY/(SIGX - SIGY))/2;
EV1   = [1;0]; EV2 = [0;1];
if abs(TAUXY) > 1.0E-4
   EV1(2) = (SIG1 - SIGX)/TAUXY;
   EV2(1) = (SIG2 - SIGY)/TAUXY;
end

