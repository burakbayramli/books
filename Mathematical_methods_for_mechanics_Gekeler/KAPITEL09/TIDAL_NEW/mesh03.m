function t1 = mesh03(p,t,GRAFIK);
% Eckart Gekeler, Universitaet Stuttgart, Release 19.9.09
% common long edges of two triangles
% are replaced by short edges

if nargin == 2, GRAFIK = 0; end
TOL = 1.0e-03; N = size(p,2); M = size(t,2);
EL1 = [t(1:3,:); t(1:2,:)]; AUX = zeros(3,M); t1 = t;
for I = 1:M
   LX         = p(1,EL1([2:4],I)) - p(1,EL1(1:3,I));
   LY         = p(2,EL1([2:4],I)) - p(2,EL1(1:3,I));
   C          = sqrt(LX.*LX + LY.*LY);
   J          = min(find(C == max(C)));
   AUX(1:2,I) = [I;C(J)];
   t(1:3,I) = EL1(J:J+2,I);  % Erste Kante ist laengste
end
for I = 2:M
   for K = 1:I-1
      if ismember(t(1,I),t(1:2,K)) &...
         ismember(t(2,I),t(1:2,K)) &...
         abs(AUX(2,I) - AUX(2,K)) < TOL
         % DREIECKE "I" und "K" haben gemeinsame laengste Kante
         Q = [t(3,I), t(3,K)];
         LX = p(1,Q(1)) - p(1,Q(2));
         LY = p(2,Q(1)) - p(2,Q(2));
         D  = sqrt(LX.*LX + LY.*LY);
         if D < AUX(2,I) - TOL
            AUX(3,I) = K;
         end
      end
   end
end
if GRAFIK == 1
   for I = 1:M
      if AUX(3,I) ~= 0
         R  = t(1:2,I);
         Q  = [t(3,I);t(3,AUX(3,I))];
         plot(p(1,R),p(2,R),'k');
         hold on
         plot(p(1,Q),p(2,Q),'--k');
         hold on
      end
   end
end
if GRAFIK == 1, pause, end
for I = 1:M
   if AUX(3,I) ~= 0
      K = AUX(3,I);
      t1(1:3,I) = [t(3,I);t(1,I);t(3,K)];
      t1(1:3,K) = [t(3,K);t(2,I);t(3,I)];
      if GRAFIK == 1
         AA = t1(1:3,I); BB = t1(1:3,K);
         fill(p(1,AA),p(2,AA),'y'), hold on
         fill(p(1,BB),p(2,BB),'g'), hold on
      end
   end
end
if GRAFIK == 1, pause, end
