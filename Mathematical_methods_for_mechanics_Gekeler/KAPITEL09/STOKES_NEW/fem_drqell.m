function [SE,ME,BE,ecode] = fem_drqell(X,Y)
% elliptische RWP, gerade quadratische Dreieckelemente
% liefert die Elementmatrizen SE(6,6) und ME(6,6) sowie
% den Elementvektor BE(6) fuer ein Dreieckeelement
% INPUT:
%       X,Y die drei Eckenkoordinatenpaare
%       ecode = 1 bei falscher Orientierung

SE = zeros(6,6); ME = zeros(6,6); BE = zeros(6,1);
S1 = [3   1   0  -4   0   0;
      1   3   0  -4   0   0;
      0   0   0   0   0   0;
     -4  -4   0   8   0   0;
      0   0   0   0   8  -8;
      0   0   0   0  -8   8];
S2 = [6   1   1  -4   0  -4;
      1   0  -1  -4   4   0;
      1  -1   0   0   4  -4;
     -4  -4   0   8  -8   8;
      0   4   4  -8   8  -8;
     -4   0  -4   8  -8   8];
S3 = [3   0   1   0   0  -4;
      0   0   0   0   0   0;
      1   0   3   0   0  -4;
      0   0   0   8  -8   0;
      0   0   0  -8   8   0;
     -4   0  -4   0   0   8];
S4 = [6  -1  -1   0  -4   0;
     -1   6  -1   0   0  -4;
     -1  -1   6  -4   0   0;
      0   0  -4  32  16  16;
     -4   0   0  16  32  16;
      0  -4   0  16  16  32];
SB = [0;  0;  0;  1;  1;  1];
ecode = 0;
X21 = X(2) - X(1); X31 = X(3) - X(1);
Y21 = Y(2) - Y(1); Y31 = Y(3) - Y(1);
DET = X21*Y31 - X31*Y21;
if DET <= 0, ecode = 1; else
  A  =  (X31*X31 + Y31*Y31)/DET;
  B  = -(X31*X21 + Y31*Y21)/DET;
  C  =  (X21*X21 + Y21*Y21)/DET;
  SE =  (A*S1 + B*S2 + C*S3)/6;
  ME =  DET*S4/360;
  BE =  DET*SB/6;
end
