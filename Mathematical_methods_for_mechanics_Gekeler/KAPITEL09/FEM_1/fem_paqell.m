function [SE,ME,BE,ecode] = fem_paqell(X,Y)
% elliptische RWP, vgl. SCHWARZ: FEM
% quadratischer Ansatz der Serendipity-Klasse
% liefert die Elementmatrizen SE(8,8) und ME(8,8) sowie
% den Elementvektor BE(8) fuer ein Parallelogrammelement
% INPUT:
%        X,Y die drei Eckenkoordinatenpaare
% OUTPUT:
%        SE, ME, MB
%        ecode = 1 bei falscher Orientierung

S1 = [52   28    23    17   -80    -6   -40     6;
      28   52    17    23   -80     6   -40    -6;
      23   17    52    28   -40     6   -80    -6;
      17   23    28    52   -40    -6   -80     6;
     -80  -80   -40   -40   160     0    80     0;
      -6    6     6    -6     0    48     0   -48;
     -40  -40   -80   -80    80     0   160     0;
       6   -6    -6     6     0   -48     0    48];
S2 = [85    0    35     0   -40   -20   -20   -40;
       0  -85     0   -35    40    40    20    20;
      35    0    85     0   -20   -40   -40   -20;
       0  -35     0   -85    20    20    40    40;
     -40   40   -20    20     0   -80     0    80;
     -20   40   -40    20   -80     0    80     0;
     -20   20   -40    40     0    80     0   -80;
     -40   20   -20    40    80     0   -80     0];
S3 = [52   17    23    28     6   -40    -6   -80;
      17   52    28    23     6   -80    -6   -40;
      23   28    52    17    -6   -80     6   -40;
      28   23    17    52    -6   -40     6   -80;
       6    6    -6    -6    48     0   -48     0;
     -40  -80   -80   -40     0   160     0    80;
      -6   -6     6     6   -48     0    48     0;
     -80  -40   -40   -80     0    80     0   160];
S4 = [6     2     3     2    -6    -8    -8     -6;
      2     6     2     3    -6    -6    -8     -8;
      3     2     6     2    -8    -6    -6     -8;
      2     3     2     6    -8    -8    -6     -6;
     -6    -6    -8    -8    32    20    16     20;
     -8    -6    -6    -8    20    32    20     16;
     -8    -8    -6    -6    16    20    32     20;
     -6    -8    -8    -6    20    16    20     32];
SB = [-1;  -1;   -1;   -1;    4;    4;    4;     4];
ecode = 0;
X21 = X(2) - X(1); X31 = X(3) - X(1);
Y21 = Y(2) - Y(1); Y31 = Y(3) - Y(1);
DET = X21*Y31 - X31*Y21;
if DET <= 0
   ecode = 1;
else
   A  =  (X31*X31 + Y31*Y31)/DET;
   B  = -(X21*X31 + Y21*Y31)/DET;
   C  =  (X21*X21 + Y21*Y21)/DET;
   SE = (A*S1 + B*S2 + C*S3)/90;
   ME = DET*S4/180;
   BE = DET*SB/12;
end
