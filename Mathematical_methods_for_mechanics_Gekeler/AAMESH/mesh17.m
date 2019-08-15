function [p1,t1] = mesh17(p,t,it);
% E. Gekeler, Universitaet Stuttgart, Release 19.1.06
% Netzverfeinerung fuer Dreieckszerlegung von Gebieten
% durch Halbierung der laengsten Seiten
% p muss 2 Zeilen haben
% t muss 3 Zeilen haben
% Bei nargin = 3 werden nur die Dreiecke mit 
% Dreiecksnrn. in it  verfeinert
% -- Vorbereitung ----------
% GRAFIK = 0/1, nein/ja
GRAFIK = 1;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if nargin == 2, tt = t;end
if nargin == 3, tt = t(:,it); end
if nargin == 3 & isempty(it),
   p1 = p; t1 = t;
   return
end

N = size(p,2); L = N; M = size(tt,2); p1 = p;
AUX = [tt;tt(1,:);zeros(3,M)]; 
% -- Berechnung der Seitenlaengen fuer jedes Dreieck
for I = 1:M
   C = abs(p1(1,AUX(2:4,I)) - p1(1,AUX(1:3,I))...
     + sqrt(-1)*(p1(2,AUX(2:4,I)) - p1(2,AUX(1:3,I))));
   AUX(5:7,I) = C';
end
pt = []; t1 = [];
for I = 1:M
   L1   = L + 1;
   AUX1 = max(AUX(5:7,I));
   J1   = min(find(AUX(5:7,I) == AUX1));
   if J1 == 1
      K = tt(1:2,I);
      ZWP = (p1(1:2,K(1)) + p1(1:2,K(2)))/2;
      D = [[tt(1,I); L1; tt(3,I)],[L1; tt(2,I); tt(3,I)]];
   end
   if J1 == 2
      K = tt(2:3,I);
      ZWP = (p1(1:2,K(1)) + p1(1:2,K(2)))/2;
      D = [[tt(1,I); tt(2,I); L1],[tt(1,I); L1; tt(3,I)]];
   end
   if J1 == 3
      K = tt([3,1],I);
      ZWP = (p1(1:2,K(1)) + p1(1:2,K(2)))/2;
      D = [[tt(1,I); tt(2,I); L1],[tt(2,I); tt(3,I); L1]];
   end
   t1 = [t1,D];
   pt = [pt,ZWP];
   L  = L + 1;
   if GRAFIK == 1, grafik1(D,[p1,pt]); end
end
% -- doppelte Knoten eliminieren, Elemente aufdatieren ----
L = size(p,2); [M1,N1] = size(t1);
[pt,I,J] = unique(pt','rows');
J = [[1:L],L+J']; C = t1(:); D = J(C);
t1 = reshape(D,M1,N1);
p1 = [p,pt']; NN = size(p1,2); p1 = [p1;[1:NN]];
if nargin == 3, MM =  setxor([1:size(t,2)],it);
   t1 = [t(:,MM),t1];
end
% -- haengende Knoten beseitigen ----------------------
II = size(p,2)+1:size(p1,2);
for K = 1:size(t1,2)
   ZWP = (p1(1:2,t1(1,K)) + p1(1:2,t1(2,K)))/2;
   DIFF = p1(1:2,II) - ZWP*ones(1,length(II));
   AUX = DIFF(1,:).^2 + DIFF(2,:).^2;
   AUX1 = min(AUX);
   if AUX1 < 100*eps
      J = min(find(AUX == AUX1)) + N; 
      t_neu = [[t1(1,K);J; t1(3,K)],[J;t1(2,K); t1(3,K)]];
      t1 = [t1,t_neu];
      if GRAFIK == 1, grafik2(t_neu,p1); end
   end
   ZWP = (p1(1:2,t1(2,K)) + p1(1:2,t1(3,K)))/2;
   DIFF = p1(1:2,II) - ZWP*ones(1,length(II));
   AUX = DIFF(1,:).^2 + DIFF(2,:).^2;
   AUX1 = min(AUX);
   if AUX1 < 100*eps
      J = min(find(AUX == AUX1)) + N; 
      t_neu = [[t1(1,K); t1(2,K); J],[t1(1,K);J; t1(3,K)]];
      t1  = [t1,t_neu];
      if GRAFIK == 1, grafik2(t_neu,p1); end
   end
   ZWP = (p1(1:2,t1(3,K)) + p1(1:2,t1(1,K)))/2;
   DIFF = p1(1:2,II) - ZWP*ones(1,length(II));
   AUX = DIFF(1,:).^2 + DIFF(2,:).^2;
   AUX1 = min(AUX);
   if AUX1 < 100*eps
      J = min(find(AUX == AUX1)) + N; 
      t_neu = [[t1(1,K);t1(2,K);J],[t1(2,K);t1(3,K);J]];
      t1 = [t1,t_neu];
      if GRAFIK == 1, grafik2(t_neu,p1); end
   end
end
p1 = p1(1:2,:);
[t1,I,J] = unique(t1','rows');
t1 = t1';

function grafik1(D,p1)
   AA = D(:,1); AA = [AA;AA(1)];
   BB = D(:,2); BB = [BB;BB(1)];
   fill(p1(1,AA),p1(2,AA),'y'), hold on
   fill(p1(1,BB),p1(2,BB),'g'), hold on
   pause(0.1)

function grafik2(C,p1)
   AA = C(:,1); AA = [AA;AA(1)];
   BB = C(:,2); BB = [BB;BB(1)];
   fill(p1(1,AA),p1(2,AA),'b'), hold on
   fill(p1(1,BB),p1(2,BB),'r'), hold on
   pause(0.1)
