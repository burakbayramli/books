function t1 = mesh27(p,e,t,SEGNR1,SEGNR2);
% Eckart Gekeler, Universitaet Stuttgart, Release 8.4.05
% fuer nichtkonvexe Gebiete
% SEGNR1 : Segmentnrn von Aussenrand
% SEGNR2 : Segmentnrn von Innenrand
% Eliminiert Dreiecke ausserhalb eines Gebietes
% mit einem Aussenrand und einem Innenrand

% -- Aussenrand ----------------
RAND1 = [];
for I = 1:length(SEGNR1)
   J     = find(e(5,:) == SEGNR1(I));
   RAND1 = [RAND1,e(1,J)];
end
N = size(t,2); AUX = zeros(1,N);
for I = 1:N
   K1 = length(find(RAND1 == t(1,I)));
   K2 = length(find(RAND1 == t(2,I)));
   K3 = length(find(RAND1 == t(3,I)));
   if K1 + K2 + K3  == 3, AUX(I) = 1; end
end
J  = find(AUX == 0); t1 = t(:,J);
K  = find(AUX == 1); t2 = t(:,K);
RICHTUNG  = zeros(1,size(t2,2));
for I = 1:size(t2,2)
   t2(:,I) = sort(t2(:,I));
   J = t2(1,I); K = t2(2,I); L = t2(3,I);
   X21 = p(1,K) - p(1,J); X31 = p(1,L) - p(1,J);
   Y21 = p(2,K) - p(2,J); Y31 = p(2,L) - p(2,J);
   DET = X21*Y31 - X31*Y21;
   if DET > 0, RICHTUNG(I) = 1; end
end
I = find(RICHTUNG == 1);
t2 = t2(:,I);
taux = [t1, t2];
taux1 = [];
% -- Innenrand ------------------------
if ~isempty(SEGNR2)
   RAND2 = [];
   for I = 1:length(SEGNR2)
      J     = find(e(5,:) == SEGNR2(I));
      RAND2 = [RAND2,e(1,I)];
   end
   N = size(taux,2); AUX = zeros(1,N);
   for I = 1:N
      K1 = length(find(RAND2 == taux(1,I)));
      K2 = length(find(RAND2 == taux(2,I)));
      K3 = length(find(RAND2 == taux(3,I)));
      if K1 + K2 + K3  == 3, AUX(I) = 1; end
   end
   J  = find(AUX == 0); t3 = taux(:,J);
   K  = find(AUX == 1); t4 = taux(:,K);
   RICHTUNG  = zeros(1,size(t4,2));
   for I = 1:size(t4,2)
      t2(:,I) = sort(t4(:,I));
      J = t4(1,I); K = t4(2,I); L = t4(3,I);
      X21 = p(1,K) - p(1,J); X31 = p(1,L) - p(1,J);
      Y21 = p(2,K) - p(2,J); Y31 = p(2,L) - p(2,J);
      DET = X21*Y31 - X31*Y21;
      if DET < 0, RICHTUNG(I) = 1; end
   end
   I = find(RICHTUNG == 1);
   t4 = t4(:,I);
   taux1 = [t3, t4];
end
t1 = [taux, taux1];
