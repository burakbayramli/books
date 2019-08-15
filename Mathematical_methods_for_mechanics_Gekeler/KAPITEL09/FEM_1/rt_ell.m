function [SE,ME,BE,ecode] = rt_ell(X,Y)
% Elliptische RWP, Rannacher-Turek-Element
% liefert die Elementmatrizen SE(4,4) und ME(4,4) sowie
% den Elementvektor BE(4)
% INPUT:
%        X,Y die drei wesentlichen Eckenkoordinatenpaare
% OUTPUT:
%        SE, ME, BE
%        ecode = 1 bei falscher Orientierung

ecode = 0;
SE1 = zeros(4,4); ME1 = SE1; BE1 = zeros(4,1);
SE2 = zeros(4,4); ME2 = SE2; BE2 = zeros(4,1);
% -- Gauss-Knoten im Einheitsdreieck ------------
s15 = sqrt(15);
a = (6+s15)/21; b = (9-2*s15)/21;
c = (6-s15)/21; d = (9+2*s15)/21;
SIG =  [1/3, a, b, a, c, d, c;
        1/3, a, a, b, c, c, d];
% -- Gauss-Gewichte im Einheitsdreieck ------------
c1 = 0.225; c2 = (155 + s15)/1200; c3 = (155-s15)/1200;
gama = [c1,c2,c2,c2,c3,c3,c3]/2;
% -- Viereck wird in zwei Dreiecke unterteilt

% -- Erstes Dreieck fuer Gauss-Integration --
XX = [X(1),X(2),X(4)]; YY = [Y(1),Y(2),Y(4)]; 
X21 = XX(2) - XX(1); X31 = XX(3) - XX(1);
Y21 = YY(2) - YY(1); Y31 = YY(3) - YY(1);
DET = X21*Y31 - X31*Y21;
% -- Transformation von S nach T fuer Knotenpunkte
TT = [X21, X31; Y21, Y31]; A = [XX(1);YY(1)];
XXX = A*ones(1,7) + TT*SIG; % xy-Koordinaten
for I = 1:size(SIG,2)
   [FF,FFX,FFY,ecode] = rt_ff(X,Y,XXX(1,I),XXX(2,I));
    SE1 = SE1 + gama(I)*(FFX*FFX' + FFY*FFY'); 
    ME1 = ME1 + gama(I)*FF*FF'; 
    BE1 = BE1 + gama(I)*FF; 
end
SE1 = SE1*DET; ME1 = ME1*DET; BE1 = BE1*DET;

% -- Zweites Dreieck fuer Gauss-Integration --
XX = [X(2),X(3),X(4)]; YY = [Y(2),Y(3),Y(4)]; 
X21 = XX(2) - XX(1); X31 = XX(3) - XX(1);
Y21 = YY(2) - YY(1); Y31 = YY(3) - YY(1);
DET = X21*Y31 - X31*Y21;
% -- Transformation von S nach T fuer Knotenpunkte
TT = [X21, X31; Y21, Y31]; A = [XX(1);YY(1)];
XXX = A*ones(1,7) + TT*SIG; 
for I = 1:size(SIG,2)
   [FF,FFX,FFY,ecode] = rt_ff(X,Y,XXX(1,I),XXX(2,I));
    SE2 = SE2 + gama(I)*(FFX*FFX' + FFY*FFY'); 
    ME2 = ME2 + gama(I)*FF*FF'; 
    BE2 = BE2 + gama(I)*FF; 
end
SE2 = SE2*DET; ME2 = ME2*DET; BE2 = BE2*DET;

SE = SE1 + SE2; ME = ME1 + ME2; BE = BE1 + BE2; 
