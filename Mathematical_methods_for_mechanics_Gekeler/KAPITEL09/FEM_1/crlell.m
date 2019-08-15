function [SE,ME,BE,ecode] = crqell(XK,YK)
% liefert die Elementmatrizen SE(3,3) und ME(3,3) sowie
% den Elementvektor BE(3) fuer 
% Crouzeix-Raviart-Element (CR-Element)
% INPUT:
%        XK,YK die drei Eckenkoordinatenpaare
%        ecode = 1 bei falscher Orientierung
S1 = [0   0   0;
      0   4  -4;
      0  -4   4];
S2 = [0  -4   4;
     -4   8  -4;
      4  -4   0];
S3 = [4  -4   0;
     -4   4   0;
      0   0   0];
S4 = [4   0   0;
      0   4   0;
      0   0   4];
SB = [1;  1;  1];
ecode = 0;
X21 = XK(2) - XK(1);
X31 = XK(3) - XK(1);
Y21 = YK(2) - YK(1);
Y31 = YK(3) - YK(1);
DET = X21*Y31 - X31*Y21;
if DET <= 0
   ecode = 1; disp(' Geometriefehler '); return
else
   A  =  (X31*X31 + Y31*Y31)/DET;
   B  = -(X31*X21 + Y31*Y21)/DET;
   C  =  (X21*X21 + Y21*Y21)/DET;
   SE = (A*S1 + B*S2 + C*S3)/2;
   ME = DET*S4/24;
   BE = DET*SB/6;
end
