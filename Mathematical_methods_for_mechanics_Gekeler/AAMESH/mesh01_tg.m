function [p1,e1,t1] = mesh01_tg(FF2,p,e,t,it)
% E. Gekeler, Universitaet Stuttgart, Release 19.1.06
% wie MESH01_T.M aber mit Grafik
% gleichmaessige Verfeinerung von Dreiecken
% p muss 2 Zeilen haben
% t muss 3 Zeilen haben
% e muss 5 Zeilen haben
% Bei nargin = 5 werden nur die Dreiecke mit 
% Dreiecksnrn. in it  verfeinert
GRAFIK = 1;
p1 = p; e1 = e; t1 = t;
if nargin == 4 tt = t; end
if nargin == 5, tt = t(:,it); end
if nargin == 5 & isempty(it), return; end

e1 = e; L = size(p,2); M = L;
if ~isempty(t)
   N = size(tt,2);  pt  = []; t1 = [];
   for I = 1:N
      L1 = L+1; L2 = L+2; L3 = L+3;
      AUX1 = [tt(1,I),      L1,      L3,  L1;
                  L1, tt(2,I),      L2,  L2;
                  L3,      L2, tt(3,I),  L3];
      t1 = [t1,AUX1];
      J  = tt(1:3,I);
      AUX  = [(p(:,J(1))+p(:,J(2)))/2, ...
              (p(:,J(2))+p(:,J(3)))/2, ...
              (p(:,J(3))+p(:,J(1)))/2];
      pt = [pt,AUX];
      L  = L + 3;
   end
end
% -- doppelte Knoten eliminieren ----------
% -- Wegen der Kantenmatrix e darf die Reihenfolge von p
% -- nicht geandert werden
[M1,N1] = size(t1);
[pt,I,J] = unique(pt','rows');
J = [[1:M],M+J']; C = t1(:); D = J(C);
t1 = reshape(D,M1,N1); pt = pt'; p1 = [p,pt];
if nargin == 5, MM =  setxor([1:size(t,2)],it);
   if GRAFIK == 1
      for J = 1:size(t1,2)
         XA = p1(1,t1(:,J)); YA = p1(2,t1(:,J));
         fill(XA,YA,'g'), hold on
         pause(0.1)
      end
   end
   t1 = [t(:,MM),t1];
end
pause
% -- haengende Knoten beseitigen ----------------------
if nargin == 5
   RR = ones(1,size(t1,2)); taux = [];
   p1 = [p1;[1:size(p1,2)]];
   II = size(p,2)+1:size(p1,2);
   for K = 1:size(t1,2)
      ZWP = (p1(1:2,t1(1,K))+p1(1:2,t1(2,K)))/2;
      DIFF = p1(1:2,II) - ZWP*ones(1,length(II));
      AUX = DIFF(1,:).^2 + DIFF(2,:).^2;
      AUX1 = min(AUX);
      J1 = min(find(AUX == AUX1)) + M; 
      Q1 = (AUX1 < 100*eps);
      ZWP = (p1(1:2,t1(2,K)) + p1(1:2,t1(3,K)))/2;
      DIFF = p1(1:2,II) - ZWP*ones(1,length(II));
      AUX = DIFF(1,:).^2 + DIFF(2,:).^2;
      AUX1 = min(AUX);
      J2 = min(find(AUX == AUX1)) + M; 
      Q2 = (AUX1 < 100*eps);
      ZWP = (p1(1:2,t1(3,K)) + p1(1:2,t1(1,K)))/2;
      DIFF = p1(1:2,II) - ZWP*ones(1,length(II));
      AUX = DIFF(1,:).^2 + DIFF(2,:).^2;
      AUX1 = min(AUX);
      J3 = min(find(AUX == AUX1)) + M; 
      Q3 = (AUX1 < 100*eps);
      if Q1 == 1 & Q2 == 0 & Q3 == 0
         C = [[t1(1,K);p1(3,J1); t1(3,K)],...
             [p1(3,J1);t1(2,K); t1(3,K)]];
         taux = [taux,C]; RR(K) = 0;
         if GRAFIK == 1
            disp(' 1,0,0')
            fill(p1(1,C(:,1)),p1(2,C(:,1)),'r'), hold on
            fill(p1(1,C(:,2)),p1(2,C(:,2)),'b'), hold on
            pause(0.1)
         end
      end
      if Q1 == 0 & Q2 == 1 & Q3 == 0
         C = [[t1(1,K); t1(2,K); p1(3,J2)],...
             [t1(1,K); p1(3,J2); t1(3,K)]];
         taux = [taux,C]; RR(K) = 0;
         if GRAFIK == 1
            disp(' 0,1,0') 
            fill(p1(1,C(:,1)),p1(2,C(:,1)),'r'), hold on
            fill(p1(1,C(:,2)),p1(2,C(:,2)),'b'), hold on
            pause(0.1)
         end
      end
      if Q1 == 0 & Q2 == 0 & Q3 == 1
         C = [[t1(1,K);t1(2,K);p1(3,J3)],...
             [t1(2,K);t1(3,K);p1(3,J3)]];
         taux = [taux,C]; RR(K) = 0;
         if GRAFIK == 1
            disp('0,0,1')
            fill(p1(1,C(:,1)),p1(2,C(:,1)),'r'), hold on
            fill(p1(1,C(:,2)),p1(2,C(:,2)),'b'), hold on
            pause(0.1)
         end
      end
      if Q1 == 1 & Q2 == 1 & Q3 == 1
         C = [[p1(3,J1);t1(2,K);p1(3,J2)]...
             [p1(3,J2);t1(3,K);p1(3,J3)]...
             [p1(3,J3);t1(1,K);p1(3,J1)]...
             [p1(3,J1);p1(3,J2);p1(3,J3)]];
         taux = [taux,C]; RR(K) = 0;
         if GRAFIK == 1
            disp(' 1,1,1') 
            fill(p1(1,C(:,1)),p1(2,C(:,1)),'r'), hold on
            fill(p1(1,C(:,2)),p1(2,C(:,2)),'b'), hold on
            fill(p1(1,C(:,3)),p1(2,C(:,3)),'y'), hold on
            fill(p1(1,C(:,4)),p1(2,C(:,4)),'g'), hold on
            pause(0.1)
         end

      end
      if Q1 == 1 & Q2 == 1 & Q3 == 0
         C = [[t1(1,K);p1(3,J1);p1(3,J2)],...
              [p1(3,J1);t1(2,K);p1(3,J2)],...
              [t1(1,K);p1(3,J2);t1(3,K)]];
         taux = [taux,C]; RR(K) = 0;
         if GRAFIK == 1
            disp('1,1,0')  
            fill(p1(1,C(:,1)),p1(2,C(:,1)),'b'), hold on
            fill(p1(1,C(:,2)),p1(2,C(:,2)),'r'), hold on
            fill(p1(1,C(:,3)),p1(2,C(:,3)),'g'), hold on
            pause(0.1)
         end 
      end
      if Q1 == 1 & Q2 == 0 & Q3 == 1
         C = [[t1(1,K);p1(3,J1);p1(3,J3)],...
              [p1(3,J1);t1(2,K);p1(3,J3)],...
              [t1(2,K);t1(3,K);p1(3,J3)]];
         taux = [taux,C]; RR(K) = 0;
         if GRAFIK == 1
            disp('1,0,1')
            fill(p1(1,C(:,1)),p1(2,C(:,1)),'b'), hold on
            fill(p1(1,C(:,2)),p1(2,C(:,2)),'r'), hold on
            fill(p1(1,C(:,3)),p1(2,C(:,3)),'g'), hold on
            pause(0.1)
         end 
      end
      if Q1 == 0 & Q2 == 1 & Q3 == 1
         C = [[t1(1,K);p1(3,J2);p1(3,J3)],...
              [p1(3,J2);t1(3,K);p1(3,J3)],...
              [t1(1,K);t1(2,K);p1(3,J2)]];
         taux = [taux,C]; RR(K) = 0;
         if GRAFIK == 1
            disp('0,1,1')
            fill(p1(1,C(:,1)),p1(2,C(:,1)),'b'), hold on
            fill(p1(1,C(:,2)),p1(2,C(:,2)),'r'), hold on
            fill(p1(1,C(:,3)),p1(2,C(:,3)),'g'), hold on
            pause(0.1)
         end 
      end
   end
   JJ = find(RR == 1);
   t1 = t1(:,JJ);
   t1 = [t1,taux]; 
   p1 = p1(1:2,:);
   [t1,I,J] = unique(t1','rows');
   t1 = t1';
end
% -- Rand verfeinern ----------------------
e1 = []; 
for I = 1:size(e,2)
   X  = p(1,e(1:2,I)); Y = p(2,e(1:2,I));
   ZWP = [(X(1) + X(2))/2;(Y(1)+Y(2))/2];
   DIFF = pt - ZWP*ones(1,size(pt,2));
   NORMDIFF = sqrt(DIFF(1,:).*DIFF(1,:) + DIFF(2,:).*DIFF(2,:));
   J = find(NORMDIFF < 100*eps);
   if length(J) > 1, disp(' Geometriefehler! ')
      return
   end
   if isempty(J)
      e1 = [e1,e(:,I)];
   else 
      J = J + M;
      E12 = [[e(1,I);J],[J;e(2,I)]];
      E34 = [[e(3,I); (e(3,I)+ e(4,I))/2],[(e(3,I)+ e(4,I))/2; e(4,I)]];
      E5  = [e(5,I),e(5,I)]; 
      AUX = [E12;E34;E5];
      e1 = [e1,AUX];
   end
end
% -- Knoten bei krummem Rand ersetzen -------------
if ~isempty(FF2)
   SEGZAHL = max(e1(5,:)); SEGZAHL = SEGZAHL(1);
   for I = 1:SEGZAHL
      J = find(e1(5,:) == I); SEGRAND = e1(3,J);
      [X,Y] = feval(FF2,I,SEGRAND);
      p1(1,e1(1,J)) = X; p1(2,e1(1,J)) = Y;
   end
end
