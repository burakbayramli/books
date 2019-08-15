function [SE,ME,BE,ecode] = bilin(X,Y)
% Elliptische RWP, bilineares Parallelogrammelement
% liefert die Elementmatrizen SE(4,4) und ME(4,4) sowie
% den Elementvektor BE(4)
% INPUT:
%        X,Y die drei wesentlichen Eckenkoordinatenpaare
% OUTPUT:
%        SE, ME, BE
%        ecode = 1 bei falscher Orientierung

ecode = 0;
SE = zeros(4,4);
ME = zeros(4,4);
BE = zeros(1,4);
S1 = [2 -2 -1  1;
     -2  2  1 -1;
     -1  1  2 -2;
      1 -1 -2  2];
S2 = [1  0 -1  0;
      0 -1  0  1;
     -1  0  1  0;
      0  1  0 -1];
S3 = [2  1 -1 -2;
      1  2 -2 -1;
     -1 -2  2  1;
     -2 -1  1  2];
S4 = [4  2  1  2;
      2  4  2  1;
      1  2  4  2;
      2  1  2  4];
SB = [1; 1; 1; 1];
X21 = X(2) - X(1);
X31 = X(3) - X(1);
Y21 = Y(2) - Y(1);
Y31 = Y(3) - Y(1);
DET = X21*Y31 - X31*Y21;
if DET <= 0
  ecode = 1;
else
  A  =  (X31*X31 + Y31*Y31);
  B  = -3*(X31*X21 + Y31*Y21);
  C  =  (X21*X21 + Y21*Y21);
  BE =  DET*SB/4;
  SE =  (A*S1 + B*S2 + C*S3)/(6*DET);
  ME =  DET*S4/36;
end
% ---------------------------------------
