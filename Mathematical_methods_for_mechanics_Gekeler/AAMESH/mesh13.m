function [midpoints,normalen] = mesh13(p,t)
% Eckart Gekeler, Universitaet Stuttgart, Release 8.4.05
% Berechnet Seitenmitten und Normalen in Seitenmitten fuer Dreiecke
% so das zwei aneinandergrenzende Kanten eine gemeinsame
% Normalenrichtung haben

N = size(t,2); midpoints = zeros(6,N); normalen = zeros(6,N);
for I = 1:N
   K = t(1:3,I); X = p(1,K); Y = p(2,K);
   X21 = X(2) - X(1); X32 = X(3) - X(2); X13 = X(1) - X(3);
   Y21 = Y(2) - Y(1); Y32 = Y(3) - Y(2); Y13 = Y(1) - Y(3);
   L1 = sqrt(X21*X21 + Y21*Y21);
   L2 = sqrt(X32*X32 + Y32*Y32);
   L3 = sqrt(X13*X13 + Y13*Y13);
   N1 = [Y21; - X21]/L1;
   N2 = [Y32; - X32]/L2;
   N3 = [Y13; - X13]/L3;
   M1 = [(X(1) + X(2))/2; (Y(1) + Y(2))/2];
   M2 = [(X(2) + X(3))/2; (Y(2) + Y(3))/2];
   M3 = [(X(3) + X(1))/2; (Y(3) + Y(1))/2];
  midpoints(:,I) = [M1;M2;M3];
  normalen(:,I)  = [N1;N2;N3];
end
for I = 1:N
   for K = 1:I-1
     AUX1  = find(t(1:3,K) == t(1,I));
     AUX2  = find(t(1:3,K) == t(2,I));
     AUX   = [AUX1,AUX2];
     if length(AUX) == 2
       normalen(1:2,I) = - normalen(1:2,I);
     end
     AUX1  = find(t(1:3,K) == t(2,I));
     AUX2  = find(t(1:3,K) == t(3,I));
     AUX   = [AUX1,AUX2];
     if length(AUX) == 2
       normalen(3:4,I) = - normalen(3:4,I);
     end
     AUX1  = find(t(1:3,K) == t(3,I));
     AUX2  = find(t(1:3,K) == t(1,I));
     AUX   = [AUX1,AUX2];
     if length(AUX) == 2
       normalen(5:6,I) = - normalen(5:6,I);
     end
   end
end
