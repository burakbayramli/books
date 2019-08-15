function [SE,ME,BE,ecode] = drlell(X,Y)
% vgl. SCHWARZ: FEM
% liefert die Elementmatrizen SE(3,3) und ME(3,3) sowie
% den Elementvektor BE(3)
% elliptische Probleme, lineare Dreieckselemente
% INPUT:
%        X,Y die drei Eckenkoordinatenpaare
%        ecode = 1 bei falscher Orientierung
% OUTPUT:
%        SE, ME, BE

SE = zeros(3,3); ME = zeros(3,3); BE = zeros(3,1);
S1 = [1  -1   0; -1   1   0;  0   0   0];
S2 = [2  -1  -1; -1   0   1; -1   1   0];
S3 = [1   0  -1;  0   0   0; -1   0   1];
S4 = [2   1   1;  1   2   1;  1   1   2];
SB    = [1;  1;  1];
ecode = 0;
X21 = X(2) - X(1); X31 = X(3) - X(1);
Y21 = Y(2) - Y(1); Y31 = Y(3) - Y(1);
DET = X21*Y31 - X31*Y21;
if DET <= 0
  ecode = 1;
else
  A =  (X31*X31 + Y31*Y31)/DET;
  B = -(X31*X21 + Y31*Y21)/DET;
  C =  (X21*X21 + Y21*Y21)/DET;
  BE  = DET*SB/6; SE = (A*S1 + B*S2 + C*S3)/2;
  ME  = DET*S4/24;
end
