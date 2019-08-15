function [SE,ME,BE,ecode] = fem_drkpla(XK,YK,E,NU,H,P)
% -- GEKELER: FINITE ELEMENTE, vgl. SCHWARZ: FEM ---------
% LIEFERT DIE ELEMENTMATRIZEN SE(9,9) UND ME(9,9) SOWIE DEN
% ELEMENTVEKTOR BE(9) FUER EIN NICHTKONFORMES DREIECKELEMENT
% KUBISCHER ANSATZ NACH ZIENKIEWICZ, PLATTENPROBLEME
% XK, YK : DIE DREI ECKENKOORDINATENPAARE
% E : ELASTIZITAETSMODUL,  NU : POISSONZAHL
% H : DICKE DES ELEMENTES,  P : BELASTUNG DES ELEMENTES
% ecode: WIRD BEI FALSCHER ORIENTIERUNG GLEICH 1 GESETZT
% ACHTUNG: Bem. auf Schwarz: S.130
%----------------------------------------------------------
SE = zeros(9,9); ME = zeros(9,9); BE = zeros(9,1);
S1 = [  80, 28,  4, -64, 40,     4,-16, -4,   8,  30,...
        -4,-44, 10,  -4, 16,     4, -8,  2,   4,   2,...
         2, -8, -2,   4, 80,   -32,  4,-16,  -4,   8,...
        24,  2, -8,  -2,  4,     2, -8, -2,   4,  32,...
         8,-16,  2,  -4,  8];
S2 = [  64, 12,  8, -32, 24,     8,-32, -4,  16,  11,...
         2,-12,  5,  -2,  0,    -4,  0,  1,  -4,   1,...
         1, -4,   1,  2, 16,   -12, -4, 16,   8,  -8,...
         8,  5, -12, -1,   6,    1, -4,  1,   2,  16,...
        -4, -8,  -3,  2,   4];
S3 = [  64, 12,  12,-32,  20,    0,-32,  0,  20,  -1,...
        11,  0,   2,  1, -12,   -1,  0,  -1,-12,   0,...
        -1,  0,   1,  2,  16,  -16,  0,  16,  0,  -4,...
        12, -2,  -4,  2,   6,   -1,  0,   1,  2,  16,...
         0,-16,  -1, -2,  12];
S4 = [  64,  8,   8,-32,  16,    8,-32,   8, 16,   5,...
         1, -4,   2,  1,  -4,   -3,  2,   5, -4,   2,...
        -3, -4,   1,  2,  16,   -8, -4,  16, -4,  -8,...
         4,  2,  -8,  2,   4,    5, -4,   1,  2,  16,...
        -4, -8,   5,  2,   4];
S5 = [  64,  8,  12,-32,  16,   -4,-32,   8, 24,   1,...
         2, -4,   2,  1,  -4,    1,  1,  11,  0,   0,...
        -4,-12,  -2,  5,  16,   -8, -4,  16, -4, -12,...
         4,  2,  -8,  2,   6,   -3,  8,   1, -1,  16,...
        -4,-12,   1,  5,   8];
S6 = [  80,  4,  28,-16,   8,   -4,-64,   4, 40,   2,...
        -4, -8,   4, -2,   4,    2,  2,  30, 16,  -8,...
         4,-44,  -4, 10,  32,  -16,  8, -16, -8,  -8,...
         8, -4,   8,  4,   4,    2, -4,  -2, -2,  80,...
         4,-32,   2,  2,  24];
S7 = [1936,208, 208,712,-212,   76,712,  76,-212, 31,...
        19,136, -38, 13,  76,   11,-24,  31,  76,-24,...
        11,136,  13,-38,1936, -416,208, 712, 136,-212,...
       100,-50,-212,-38,  62,   31,136,  25, -38,1936,...
       208,-416, 31,-50, 100];
SB = [   8,   1,  1,  8,  -2,    1,  8,   1,  -2];
ecode = 0;
X21 = XK(2) - XK(1); X31 = XK(3) - XK(1);
Y21 = YK(2) - YK(1); Y31 = YK(3) - YK(1);
D = E * H * H * H /(12*(1 - NU * NU));
DET = X21 * Y31 - X31 * Y21;
if DET <= 0
   ecode = 1;
else
   DET3 = DET * DET * DET * 24/D;
   H1 = X31 * X31 + Y31 * Y31;
   H2 = X21 * X31 + Y21 * Y31;
   H3 = X21 * X21 + Y21 * Y21;
   A1 = H1 * H1 / DET3;
   A2 = -4 * H2 * H1 / DET3;
   A3 = 2 * (H2 * H2 + NU * DET * DET) / DET3;
   A4 = (4 * H2 * H2 + 2 * (1 - NU) * DET * DET) / DET3;
   A5 = -4 * H2 * H3 / DET3;
   A6 = H3 * H3 / DET3;
   IJ = 0;
   for  I = 1:9
      for J = I:9
         IJ = IJ + 1;
         SE(I,J) = A1 * S1(IJ) + A2 * S2(IJ) + A3 * S3(IJ)...
                   + A4 * S4(IJ) + A5 * S5(IJ) + A6 * S6(IJ);
         SE(J,I) = SE(I,J);
         ME(I,J) = DET * S7(IJ) / 20160;
         ME(J,I) = ME(I,J);
      end
      BE(I) = P * DET * SB(I) / 48;
   end
   for J = [2,5,8]
      for I = 1:9
         H1        = X21 * SE(I,J) + X31 * SE(I,J+1);
         SE(I,J+1) = Y21 * SE(I,J) + Y31 * SE(I,J+1);
         SE(I,J)   = H1;
         H1        = X21 * ME(I,J) + X31 * ME(I,J+1);
         ME(I,J+1) = Y21 * ME(I,J) + Y31 * ME(I,J+1);
         ME(I,J)   = H1;
      end
   end
   for I = [2,5,8]
      for J = 1:9
         H1        = X21 * SE(I,J) + X31 * SE(I+1,J);
         SE(I+1,J) = Y21 * SE(I,J) + Y31 * SE(I+1,J);
         SE(I,J)   = H1;
         H1        = X21 * ME(I,J) + X31 * ME(I+1,J);
         ME(I+1,J) = Y21 * ME(I,J) + Y31 * ME(I+1,J);
         ME(I,J)   = H1;
      end
      H1 = X21 * BE(I) + X31 * BE(I+1);
      BE(I+1) = Y21 * BE(I) + Y31 * BE(I+1);
      BE(I) = H1;
   end
end
