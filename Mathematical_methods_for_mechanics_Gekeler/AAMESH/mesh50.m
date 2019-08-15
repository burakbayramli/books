function t1 = mesh50(p,t)
% E. Gekeler, Universitaet Stuttgart, Release 19.1.06
% beseitigt haengende Knoten in Dreiecken
% t hat drei Zeilen
% jeder Knoten und jedes Dreieck darf nur einmal vorkommen
% Option: GRAFIK ein/aus (0/1) 
GRAFIK = 0;

RR = ones(1,size(t,2)); taux = [];
M = size(p,2);
for K = 1:size(t,2)
   ZWP1 = (p(1:2,t(1,K))+p(1:2,t(2,K)))/2;
   DIFF = p(1:2,:) - ZWP1*ones(1,M);
   AUX  = DIFF(1,:).^2 + DIFF(2,:).^2;
   AUX1 = min(AUX);
   J1   = min(find(AUX == AUX1)); 
   Q1   = (AUX1 < 100*eps);

   ZWP2 = (p(1:2,t(2,K)) + p(1:2,t(3,K)))/2;
   DIFF = p(1:2,:) - ZWP2*ones(1,M);
   AUX  = DIFF(1,:).^2 + DIFF(2,:).^2;
   AUX1 = min(AUX);
   J2   = min(find(AUX == AUX1)); 
   Q2   = (AUX1 < 100*eps);
   ZWP3 = (p(1:2,t(3,K)) + p(1:2,t(1,K)))/2;
   DIFF = p(1:2,:) - ZWP3*ones(1,M);
   AUX  = DIFF(1,:).^2 + DIFF(2,:).^2;
   AUX1 = min(AUX);
   J3   = min(find(AUX == AUX1)); 
   Q3   = (AUX1 < 100*eps);
   if Q1 == 1 & Q2 == 0 & Q3 == 0
      C = [[t(1,K);     J1; t(3,K)],...
           [    J1; t(2,K); t(3,K)]];
      taux = [taux,C]; RR(K) = 0;
      if GRAFIK == 1
         disp(' 1,0,0')
         fill(p(1,C(:,1)),p(2,C(:,1)),'r'), hold on
         fill(p(1,C(:,2)),p(2,C(:,2)),'b'), hold on
         pause
      end
   end
   if Q1 == 0 & Q2 == 1 & Q3 == 0
      C = [[t(1,K); t(2,K);     J2],...
           [t(1,K);     J2; t(3,K)]];
      taux = [taux,C]; RR(K) = 0;
      if GRAFIK == 1
         disp(' 0,1,0')
         fill(p(1,C(:,1)),p(2,C(:,1)),'r'), hold on
         fill(p(1,C(:,2)),p(2,C(:,2)),'b'), hold on
         pause
      end
   end
   if Q1 == 0 & Q2 == 0 & Q3 == 1
      C = [[t(1,K); t(2,K); J3],...
           [t(2,K); t(3,K); J3]];
      taux = [taux,C]; RR(K) = 0;
      if GRAFIK == 1
         disp(' 0,0,1')
         fill(p(1,C(:,1)),p(2,C(:,1)),'r'), hold on
         fill(p(1,C(:,2)),p(2,C(:,2)),'b'), hold on
         pause
      end
   end
   if Q1 == 1 & Q2 == 1 & Q3 == 1
      C = [[J1; t(2,K);  J2],...
           [J2; t(3,K); J3],...
           [J3; t(1,K); J1],...
           [J1;     J2; J3]];
      taux = [taux,C]; RR(K) = 0;
   end
   if Q1 == 1 & Q2 == 1 & Q3 == 0
      C = [[t(1,K);     J1;     J2],...
           [    J1; t(2,K);     J2],...
           [t(1,K);     J2; t(3,K)]];
      taux = [taux,C]; RR(K) = 0;
   end
   if Q1 == 1 & Q2 == 0 & Q3 == 1
      C = [[t(1,K);     J1; J3],...
           [    J1; t(2,K); J3],...
           [t(2,K); t(3,K); J3]];
      taux = [taux,C]; RR(K) = 0;
   end
   if Q1 == 0 & Q2 == 1 & Q3 == 1
      C = [[t(1,K);     J2; J3],...
           [    J2; t(3,K); J3],...
           [t(1,K); t(2,K); J2]];
      taux = [taux,C]; RR(K) = 0;
   end
end
JJ = find(RR == 1); t1 = t(:,JJ); t1 = [t1,taux]; 
[t1,I,J] = unique(t1','rows'); t1 = t1';
