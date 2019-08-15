function ELEMENTE1 = test(p,ELEMENTE)
% Ordnet die Dreiecke in der richtigen Reihenfolge
% so dass Determinante positiv

ELEMENTE1 = ELEMENTE;
for I = 1:size(ELEMENTE1,2)
   K = ELEMENTE1(1:3,I);
   X = p(1,K); Y = p(2,K);
   X21 = X(2) - X(1); X31 = X(3) - X(1);
   Y21 = Y(2) - Y(1); Y31 = Y(3) - Y(1);
   DET = X21*Y31 - X31*Y21;
   if DET <= 0,
       ecode = 1;
       AUX = ELEMENTE1(1,I);
       ELEMENTE1(1,I) = ELEMENTE1(2,I);
       ELEMENTE1(2,I) = AUX;
   end
end
