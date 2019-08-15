function [p,e,t,RAND] = prepar3(p,e,t,FF2,SEGNR)
% partial refinement for backfacing step
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
K = find(p(2,:) > 0.02 & p(2,:) < 0.03);
L1= ismember(t(1,:),K); L1 = find(L1 == 1);
L2= ismember(t(2,:),K); L2 = find(L2 == 1);
L3= ismember(t(3,:),K); L3 = find(L3 == 1);
L = unique([L1,L2,L3]);
[p,e,t] = refinemesh(FF2,p,e,t,L','regular');

% -- Order boundary !!! -----------------------------
LL = max(e(5,:)); f = [];
for I = 1:LL
   J = find(e(5,:) == I); EE = e(:,J);
   [U,K] = sort(EE(3,:)) ; EE = EE(:,K);
   f = [f,EE];
end
e = f;
% -- Order boundary without inner segments ---------------
RAND = [];
if ~isempty(SEGNR)
   for I = 1:length(SEGNR)
      J  = find(e(5,:) == SEGNR(I)); EE = e(:,J);
      [U,K] = sort(EE(3,:)); EE = EE(:,K);
      RAND = [RAND,EE];
   end
end
IP = [];
INNERP = 0;
if INNERP == 1
   % -- Innere Punkte --------------
   LP = size(p,2); AUX = zeros(1,LP);
   for I = 1:LP
      if isempty(find(e(1,:) == I)), AUX(I) = 1; end
   end
   INNERPKTE = find(AUX == 1); IP = INNERPKTE;
end
save daten p e t IP RAND
Grafik = 1;
if Grafik == 1
   clf, hold on
   X = p(1,:); Y = p(2,:); Z1 = zeros(1,length(X));
   trimesh(t(1:3,:)',X,Y,Z1,'edgecolor','g'), hold on
   %trimesh(t(1:3,L)',X,Y,Z1,'edgecolor','b'), hold on
   %plot(p(1,K),p(2,K),'k*'), hold on
   for I = 1:size(e,2)
      A = [p(1,e(1,I));p(1,e(2,I))];
      B = [p(2,e(1,I));p(2,e(2,I))];
      plot(A,B,'r','linewidth',2), hold on
   end
   axis equal, axis  manual,grid on
end
