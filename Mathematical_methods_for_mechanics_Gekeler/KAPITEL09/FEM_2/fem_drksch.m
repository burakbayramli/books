function [SE,ME,ecode] = fem_drksch(XK,YK,NU)
% LIEFERT DIE ELEMENTMATRIZEN  SE(18,18)  UND  ME(18,18)  FUER
% EIN DREIECKELEMENT, VOLLSTAENDIGER KUBISCHER ANSATZ, KONDEN-
% SATION DER KNOTENVARIABLEN IM SCHWERPUNKT, SCHEIBENPROBLEM
% KEINE BERUECKSICHTIGUNG DES FAKTORS  E * H / (1 - NU * NU)
% XK, YK : DIE DREI ECKENKOORDINATENPAARE
% NU : POISSON-ZAHL
% ecode: WIRD BEI FALSCHER ORIENTIERUNG GLEICH 1 GESETZT

S1 = [199,   3,  35,   1,  -2,   5,  70,  -2, -20,  -270;
        3,   3,   0,  -3,   0,   0,   0,   0,   0,     0;
       35,   0,   7,    5,  -1,   1,  14,  -1,  -4,  -54;
        1,  -3,   5,  199, -38,  35,  70,  22, -20, -270;
       -2,   0,  -1,  -38,  10,  -7, -14,  -5,   4,   54;
        5,   0,   1,   35,  -7,   7,  14,   5,  -4,  -54;
       70,   0,  14,   70, -14,  14,  49,   7, -14, -189;
       -2,   0,  -1,   22,  -5,   5,   7,   7,  -2,  -27;
      -20,   0,  -4,  -20,   4,  -4, -14,  -2,   4,   54;
     -270,   0, -54, -270,  54, -54,-189, -27,  54,  729];
S2 = [349,  13,  49,  -89,  10,   3,  91,   3, -26, -351;
       49,   3,   5,  -29,   4,  -1,   7,   1,  -2,  -27;
       13,   5,   3,    7,  -2,   1,   7,  -1,  -2,  -27;
       91,   7,   7,   49, -14,  21,  49,  21, -14, -189;
      -26,  -2,  -2,  -14,   4,  -6, -14,  -6,   4,   54;
        3,  -1,   1,   57,  -6,  11,  21,   7,  -6,  -81;
      -89,   7, -29,  229, -50,  57,  49,  57, -14, -189;
        3,   1,  -1,   57, -12,  13,  21,  11,  -6,  -81;
       10,  -2,   4,  -50,  10, -12, -14,  -6,   4,   54;
     -351, -27, -27, -189,  54, -81,-189, -81,  54,  729];
S3 = [199,  35,   3,   70, -20,  -2,   1,   5,  -2, -270;
       35,   7,   0,   14,  -4,  -1,   5,   1,   -1, -54;
        3,   0,   3,    0,   0,   0,  -3,   0,    0,   0;
       70,  14,   0,   49, -14,   7,  70,  14,  -14,-189;
      -20,  -4,   0,  -14,   4,  -2, -20,  -4,    4,  54;
       -2,  -1,   0,    7,  -2,   7,  22,   5,   -5, -27;
        1,   5,  -3,   70, -20,  22, 199,  35,  -38,-270;
        5,   1,   0,   14,  -4,   5,  35,   7,   -7, -54;
       -2,  -1,   0,  -14,   4,  -5, -38,  -7,   10,  54;
     -270, -54,   0, -189,  54, -27,-270, -54,   54, 729];
S4 = [626,  53,  53,   14,  -4, -13,  14, -13,   -4, 270;
       53,   8,   2,   17,  -4,  -1, -13,  -2,    3,  27;
       53,   2,   8,  -13,   3,  -2,  17,  -1,   -4,  27;
       14,  17, -13,  626,-106,  53,  14,  17,   -4, 270;
       -4,  -4,   3, -106,  20, -10,  -4,  -4,    1, -54;
      -13,  -1,  -2,   53, -10,   8,  17,   5,   -4,  27;
       14, -13,  17,   14,  -4,  17, 626,  53, -106, 270;
      -13,  -2,  -1,   17,  -4,   5,  53,   8,  -10,  27;
       -4,   3,  -4,   -4,   1,  -4,-106, -10,   20, -54;
      270,  27,  27,  270, -54,  27, 270,  27,  -54,1458];
ecode = 0;
SE = zeros(20,20); ME = zeros(20,20);
MU = (1 - NU) * 0.5;
X21 = XK(2) - XK(1); X31 = XK(3) - XK(1);
Y21 = YK(2) - YK(1); Y31 = YK(3) - YK(1);
DET = X21 * Y31 - X31 * Y21;
if DET <= 0
   ecode = 1;
else
   A1 = (MU * X31 * X31 + Y31 * Y31) / (DET * 180);
   B1 =-(MU * X21 * X31 + Y21 * Y31) / (DET * 360);
   C1 = (MU * X21 * X21 + Y21 * Y21) / (DET * 180);
   A2 = (X31 * X31 + MU * Y31 * Y31) / (DET * 180);
   B2 =-(X21 * X31 + MU * Y21 * Y31) / (DET * 360);
   C2 = (X21 * X21 + MU * Y21 * Y21) / (DET * 180);
   A3 = -(1 + NU) * X31 * Y31 / (DET * 360);
   B3 = (NU * X21 * Y31 + MU * X31 * Y21) / (DET * 360);
   C3 = (NU * X31 * Y21 + MU * X21 * Y31) / (DET * 360);
   D3 = -(1 + NU) * X21 * Y21 / (DET * 360);
   for I = 1:10
      II = I + I;
      IIM1 = II - 1;
      for J = 1:10
         JJ = J + J;
         JJM1 = JJ - 1;
         SE(IIM1,JJM1) = A1 * S1(I,J) + B1 * (S2(I,J) + S2(J,I))...
                         + C1 * S3(I,J);
         SE(II,JJ) = A2 * S1(I,J) + B2 * (S2(I,J) + S2(J,I))...
                     + C2 * S3(I,J);
         SE(IIM1,JJ) = A3 * S1(I,J) + B3 * S2(I,J) + C3 * S2(J,I)...
                     + D3 * S3(I,J);
         SE(JJ,IIM1) = SE(IIM1,JJ);
         ME(IIM1,JJM1) = DET * S4(I,J) / 10080;
         ME(II,JJ) = ME(IIM1,JJM1);
         ME(IIM1,JJ) = 0;
         ME(II,JJM1) = 0;
      end
   end
   for J = [3,9,15]
      for I = 1:20
         AUX = X21 * SE(I,J) + X31 * SE(I,J+2);
         SE(I,J+2) = Y21 * SE(I,J) + Y31 * SE(I,J+2);
         SE(I,J) = AUX;
         AUX = X21 * SE(I,J+1) + X31 * SE(I,J+3);
         SE(I,J+3) = Y21 * SE(I,J+1) + Y31 * SE(I,J+3);
         SE(I,J+1) = AUX;
         AUX = X21 * ME(I,J) + X31 * ME(I,J+2);
         ME(I,J+2) = Y21 * ME(I,J) + Y31 * ME(I,J+2);
         ME(I,J) = AUX;
         AUX = X21 * ME(I,J+1) + X31 * ME(I,J+3);
         ME(I,J+3) = Y21 * ME(I,J+1) + Y31 * ME(I,J+3);
         ME(I,J+1) = AUX;
      end
   end
   for I = [3,9,15]
      for J = 1:20
         AUX = X21 * SE(I,J) + X31 * SE(I+2,J);
         SE(I+2,J) = Y21 * SE(I,J) + Y31 * SE(I+2,J);
         SE(I,J) = AUX;
         AUX = X21 * SE(I+1,J) + X31 * SE(I+3,J);
         SE(I+3,J) = Y21 * SE(I+1,J) + Y31 * SE(I+3,J);
         SE(I+1,J) = AUX;
         AUX = X21 * ME(I,J) + X31 * ME(I+2,J);
         ME(I+2,J) = Y21 * ME(I,J) + Y31 * ME(I+2,J);
         ME(I,J) = AUX;
         AUX = X21 * ME(I+1,J) + X31 * ME(I+3,J);
         ME(I+3,J) = Y21 * ME(I+1,J) + Y31 * ME(I+3,J);
         ME(I+1,J) = AUX;
      end
   end
   % ---------------------------------------------------------
   % KONDENSATIONSSCHRITTE FUER DIE VARIABLEN IM SCHWERPUNKT
   % ---------------------------------------------------------
   for  I = 1:2
      M = 21 - I;
      M1 = M - 1;
      for J = 1:M1
         SIG(J) = - SE(M,J) / SE(M,M);
      end
      for J = 1:M1
         for K = 1:J
            SE(J,K) = SE(J,K) + SE(M,J) * SIG(K);
            SE(K,J) = SE(J,K);
            ME(J,K) = ME(J,K) + SIG(J) * ME(M,K) + ME(M,J) * SIG(K)...
                   + SIG(J) * SIG(K) * ME(M,M);
            ME(K,J) = ME(J,K);
         end
      end
   end
end
SE = SE(1:18,1:18); ME = ME(1:18,1:18);
