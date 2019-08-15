function [SE,ME,CE,DE,ecode] = mini(XK,YK)
% computes element matrices SE(3,3),CE(3,3),DE(3,3)
% for the Mini element (condensation)
% linear Stokes problem
% INPUT:
%        XK,YK coordinates of vertices
% ecode = 1 in wrong orientation

S1 = [...
  10, -10,   0,   0;
 -10,  10,   0,   0;
   0,   0,   0,   0;
   0,   0,   0,  81];
S2 = [...
  20, -10, -10,   0;
 -10,   0,  10,   0;
 -10,  10,   0,   0;
   0,   0,   0,  81];
S3 = [...
  10,   0, -10,   0;
   0,   0,   0,   0;
 -10,   0,  10,   0;
   0,   0,   0,  81];
S4 = [2   1   1;  1   2   1;  1   1   2];
C1 = [...
 -20, -20, -20;
  20,  20,  20;
   0,   0,   0;
  27, -27,   0];
C2 = [...
 -20, -20, -20;
   0,   0,   0;
  20,  20,  20;
  27,   0, -27];

ecode = 0;
X21 = XK(2) - XK(1); X31 = XK(3) - XK(1); X32 = XK(3) - XK(2);
Y21 = YK(2) - YK(1); Y31 = YK(3) - YK(1); Y32 = YK(3) - YK(2);
DET = X21*Y31 - X31*Y21;
if DET <= 0
   ecode = 1;
else
   A  =  (X31*X31 + Y31*Y31)/DET;
   B  = -(X31*X21 + Y31*Y21)/DET;
   C  =  (X21*X21 + Y21*Y21)/DET;
   SE =  (A*S1 + B*S2 + C*S3)/20;
   ME  = DET*S4/24;
   CE =  (Y31*C1 - Y21*C2)/120;
   DE =  (X21*C2 - X31*C1)/120;
end
% -- Kondensation ----------------------------------------
CE = CE - SE(:,4)*CE(4,:)/SE(4,4);
DE = DE - SE(:,4)*DE(4,:)/SE(4,4);
SE = SE - SE(:,4)*SE(4,:)/SE(4,4);
M = [1:3];
SE = SE(M,M);
CE = CE(M,:);
DE = DE(M,:);
