function [SE,ME,BE] = balken1(X,H,B,Parmeter);
% Balken in spezieller Lage
% INPUT:
%       X Koordinaten der beiden Eckpunkte auf X-Achse
%       H Hoehe des rechteckigen Querschnittes
%       B Breite des rechteckigen Querschnittes
%       E Elastizitaetsmodul
% OUTPUT:
% SE_1(4,4): Steifigkeitsmatrix fuer 1. Ableitung
% SE(4,4)  : Steifigkeitsmatrix fuer 2. Ableitung
% ME(4,4)  : Massenmatrix
% BE(4,1)  : Lastvektor

E   = Parmeter(1); I = Parmeter(2);
L   = X(2) - X(1);
% not used
%SE_1 = [ 36     3*L     -36    3*L;
%        3*L   4*L*L    -3*L   -L*L;
%       - 36    -3*L      36    -3*L;
%        3*L    -L*L    -3*L   4*L*L];
%SE_1 = SE_1/(30*L);

SE = [  6     3*L     -6     3*L;
        3*L   2*L*L   -3*L   1*L*L;
       -6    -3*L      6    -3*L;
        3*L   1*L*L   -3*L   2*L*L];
SE = 2*SE/L^3;
ME = [156    22*L     54    -13*L;
       22*L   4*L*L   13*L   -3*L*L;
       54    13*L    156    -22*L;
      -13*L  -3*L*L  -22*L    4*L*L];
ME = L*ME/420;

BE = [6, L, 6, -L]'; BE = L*BE/12;
FAC1 = E*B*H*H*H/12;
if I ~= 0 FAC1 = E*I; end
FAC2 = B*H;
SE = FAC1*SE; ME = FAC2*ME;
