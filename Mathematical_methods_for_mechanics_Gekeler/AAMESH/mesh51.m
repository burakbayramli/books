function [p1,t1,q1] = mesh51(p,t,q)
% beseitigt haengende Knoten in Vierecken
% t hat drei Zeilen
% q hat vier Zeilen
% jeder Knoten und jedes Dreieck darf nur einmal vorkommen
% Wenn keine Dreiecke vorkommen, muss t = [] gesetzt werden
% Es koennen neue Dreiecke hinzukommen
% Option: GRAFIK ein/aus (0/1) 
GRAFIK = 0;

qaux = []; taux = []; paux = [];
QQ = ones(1,length(q));
M = size(p,2); JJ = M;
for K = 1:size(q,2)
   ZWP1 = (p(1:2,q(1,K))+p(1:2,q(2,K)))/2;
   DIFF = p(1:2,:) - ZWP1*ones(1,M);
   AUX  = DIFF(1,:).^2 + DIFF(2,:).^2;
   AUX1 = min(AUX);
   J1   = min(find(AUX == AUX1)); 
   Q1   = (AUX1 < 100*eps);

   ZWP2 = (p(1:2,q(2,K)) + p(1:2,q(3,K)))/2;
   DIFF = p(1:2,:) - ZWP2*ones(1,M);
   AUX  = DIFF(1,:).^2 + DIFF(2,:).^2;
   AUX1 = min(AUX);
   J2   = min(find(AUX == AUX1)); 
   Q2   = (AUX1 < 100*eps);

   ZWP3 = (p(1:2,q(3,K)) + p(1:2,q(4,K)))/2;
   DIFF = p(1:2,:) - ZWP3*ones(1,M);
   AUX  = DIFF(1,:).^2 + DIFF(2,:).^2;
   AUX1 = min(AUX);
   J3   = min(find(AUX == AUX1)); 
   Q3   = (AUX1 < 100*eps);

   ZWP4 = (p(1:2,q(4,K)) + p(1:2,q(1,K)))/2;
   DIFF = p(1:2,:) - ZWP4*ones(1,M);
   AUX  = DIFF(1,:).^2 + DIFF(2,:).^2;
   AUX1 = min(AUX);
   J4   = min(find(AUX == AUX1)); 
   Q4   = (AUX1 < 100*eps);

   if Q1 == 1 & Q2 == 0 & Q3 == 0 & Q4 == 0
      C = [    J1,     J1,     J1; 
           q(2,K), q(3,K), q(4,K);
           q(3,K), q(4,K), q(1,K)];
      taux = [taux,C]; QQ(K) = 0;
      if GRAFIK == 1
         disp(' 1,0,0,0')
         fill(p(1,C(:,1)),p(2,C(:,1)),'r'), hold on
         fill(p(1,C(:,2)),p(2,C(:,2)),'b'), hold on
         fill(p(1,C(:,3)),p(2,C(:,3)),'y'), hold on
         pause
      end
   end
   if Q1 == 0 & Q2 == 1 & Q3 == 0 & Q4 == 0
      C = [   J2,     J2,     J2; 
          q(3,K), q(4,K), q(1,K);
          q(4,K), q(1,K), q(2,K)];
      taux = [taux,C]; QQ(K) = 0;
      if GRAFIK == 1
         disp(' 0,1,0,0')
         fill(p(1,C(:,1)),p(2,C(:,1)),'r'), hold on
         fill(p(1,C(:,2)),p(2,C(:,2)),'b'), hold on
         fill(p(1,C(:,3)),p(2,C(:,3)),'y'), hold on
         pause
      end
   end
   if Q1 == 0 & Q2 == 0 & Q3 == 1 & Q4 == 0
      C = [    J3,     J3,     J3; 
           q(4,K), q(1,K), q(2,K);
           q(1,K), q(2,K), q(3,K)];
      taux = [taux,C]; QQ(K) = 0;
      if GRAFIK == 1
         disp(' 0,0,1,0')
         fill(p(1,C(:,1)),p(2,C(:,1)),'r'), hold on
         fill(p(1,C(:,2)),p(2,C(:,2)),'b'), hold on
         fill(p(1,C(:,3)),p(2,C(:,3)),'y'), hold on
         pause
      end
   end
   if Q1 == 0 & Q2 == 0 & Q3 == 0 & Q4 == 1
      C = [    J4,     J4,     J4; 
           q(1,K), q(2,K), q(3,K);
           q(2,K), q(3,K), q(4,K)];
      taux = [taux,C]; QQ(K) = 0;
      if GRAFIK == 1
         disp(' 0,0,0,1')
         fill(p(1,C(:,1)),p(2,C(:,1)),'r'), hold on
         fill(p(1,C(:,2)),p(2,C(:,2)),'b'), hold on
         fill(p(1,C(:,3)),p(2,C(:,3)),'y'), hold on
         pause
      end
   end
   if Q1 == 1 & Q2 == 1 & Q3 == 0 & Q4 == 0
      C = [    J1,     J1,     J1,     J2;
           q(2,K),     J2, q(4,K), q(3,K);
               J2, q(4,K), q(1,K), q(4,K)];
      taux = [taux,C]; QQ(K) = 0;
   end
   if Q1 == 0 & Q2 == 1 & Q3 == 1 & Q4 == 0
      C = [    J2,     J2,     J2,     J3;
           q(1,K),     J3, q(3,K), q(4,K);
           q(2,K), q(1,K),     J3, q(1,K)];
      taux = [taux,C]; QQ(K) = 0;
   end 
   if Q1 == 0 & Q2 == 0 & Q3 == 1 & Q4 == 1
      C = [    J3,     J3,     J3,     J4;
           q(4,K),     J4, q(2,K), q(1,K);
               J4, q(2,K), q(3,K), q(2,K)];
      taux = [taux,C]; QQ(K) = 0;
   end
   if Q1 == 1 & Q2 == 0 & Q3 == 0 & Q4 == 1
      C = [    J4,     J4,     J4,     J1;
           q(1,K),     J1, q(3,K), q(2,K);
               J1, q(3,K), q(4,K), q(3,K)];
      taux = [taux,C]; QQ(K) = 0;
   end
   if Q1 == 1 & Q2 == 0 & Q3 == 1 & Q4 == 0
      D = [    J1,     J1;
           q(2,K),     J3;
           q(3,K), q(4,K);
               J3, q(1,K)];
      qaux = [qaux,D]; QQ(K) = 0;
   end
   if Q1 == 0 & Q2 == 1 & Q3 == 0 & Q4 == 1
      D = [    J2,     J2;
           q(3,K),     J4;
           q(4,K), q(1,K);
               J4, q(2,K)];
      qaux = [qaux,D]; QQ(K) = 0;
   end
   if Q1 == 1 & Q2 == 1 & Q3 == 1 & Q4 == 0
      C = [    J2, J2,     J2;
               J1, J3, q(3,K);
           q(2,K), J1,     J3];
      D = [J1; J3 ; q1(4,K); q1(1,K)]; 
      taux = [taux,C]; qaux = [qaux,D]; QQ(K) = 0;
   end
   if Q1 == 0 & Q2 == 1 & Q3 == 1 & Q4 == 1
      C = [    J3, J3,      J3;
           q(4,K), J4,      J2;
               J4, J2, q(3,K)];
      D = [J2; J4; q(1,K); q(2,K)]; 
      taux = [taux,C]; qaux = [qaux,D]; QQ(K) = 0;
   end
   if Q1 == 1 & Q2 == 0 & Q3 == 1 & Q4 == 1
      C = [    J4,  J1,     J4;
           q(4,K),  J1,     J3;
               J1,  J3, q(4,K)];
      D = [J3; J1; q(2,K); q(3,K)]; 
      taux = [taux,C]; qaux = [qaux,D]; QQ(K) = 0;
   end
   if Q1 == 1 & Q2 == 1 & Q3 == 0 & Q4 == 1
      C = [    J1,  J1,     J1;
           q(2,K),  J2,     J4;
               J2,  J4, q(1,K)];
      D = [J4; J2; q(3,K); q(4,K)]; 
      taux = [taux,C]; qaux = [qaux,D]; QQ(K) = 0;
   end
   if Q1 == 1 & Q2 == 1 & Q3 == 0 & Q4 == 1
      C = [    J1, J1,     J1;
           q(2,K), J2,     J4;
               J2, J4, q(1,K)];
      D = [J4; J2;q(3,K); q(4,K)]; 
      taux = [taux,C]; qaux = [qaux,D]; QQ(K) = 0;
   end
   if Q1 == 1 & Q2 == 1 & Q3 == 1 & Q4 == 1
      JJ = JJ + 1;
      CENTER = (p(:,q(1,K)) + p(:,q(2,K)) + p(:,q(3,K)) + p(:,q(4,K)))/2;
      D = [    JJ,     JJ,     JJ,     JJ;
               J1,     J2,     J3,     J4;
           q(2,K), q(3,K), q(4,K), q(1,K);
                J2,      J3       J4,      J1];
      qaux = [qaux,D]; paux = [paux,CENTER]; QQ(K) = 0;
   end
end
KK = find(QQ == 1); q1 = q(:,KK); q1 = [q1,qaux];
[q1,I,J] = unique(q1','rows'); q1 = q1';
t1 = [t,taux];
p1 = [p,paux];
