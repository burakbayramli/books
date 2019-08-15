function [p1,e1,t1] = mesh17(p,e,t,it,GRAFIK);
% E. Gekeler, Universitaet Stuttgart, Release 19.1.06
% Netzverfeinerung fuer Dreieckszerlegung von Gebieten
% durch Halbierung der laengsten Seiten
% p muss 2 Zeilen haben
% t muss 3 Zeilen haben
% Bei nargin = 4 werden nur die Dreiecke mit 
% Dreiecksnrn. in it  verfeinert
% -- Vorbereitung ----------
p1 = p; e1 = e; t1 = t;
if nargin == 3, tt = t;       GRAFIK = 0; end
if nargin == 4, tt = t(:,it); GRAFIK = 0; end
if nargin == 5, tt = t(:,it); end
if nargin == (4 | 5) & isempty(it), return, end
N = size(p,2); L = N; M = size(tt,2); p1 = p;
AUX = [tt;tt(1,:);zeros(3,M)]; 
% -- Berechnung der Seitenlaengen fuer jedes Dreieck
for I = 1:M
   C = abs(p1(1,AUX(2:4,I)) - p1(1,AUX(1:3,I))...
     + sqrt(-1)*(p1(2,AUX(2:4,I)) - p1(2,AUX(1:3,I))));
   AUX(5:7,I) = C.';
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
if nargin == 4 | 5, MM =  setxor([1:size(t,2)],it);
   t1 = [t(:,MM),t1];
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
[p1,t1] = mesh04(p1,t1);
t1 = mesh50(p1,t1,GRAFIK);
p1 = p1(1:2,:);
[t1,I,J] = unique(t1','rows');
t1 = t1';
% -- Rand verfeinern ----------------------
e1 = []; 
for I = 1:size(e,2)
   X  = p(1,e(1:2,I)); Y = p(2,e(1:2,I));
   ZWP = [(X(1) + X(2))/2;(Y(1)+Y(2))/2];
   DIFF = p1 - ZWP*ones(1,size(p1,2));
   NORMDIFF = sqrt(DIFF(1,:).*DIFF(1,:) + DIFF(2,:).*DIFF(2,:));
   J = find(NORMDIFF < 100*eps);
   if length(J) > 1, disp(' Geometriefehler! ')
      return
   end
   if isempty(J)
      e1 = [e1,e(:,I)];
   else 
      E12 = [[e(1,I);J],[J;e(2,I)]];
      E34 = [[e(3,I); (e(3,I)+ e(4,I))/2],[(e(3,I)+ e(4,I))/2; e(4,I)]];
      E5  = [e(5,I),e(5,I)]; 
      AUX = [E12;E34;E5];
      e1 = [e1,AUX];
   end
end

function grafik1(D,p1)
   AA = D(:,1); AA = [AA;AA(1)];
   BB = D(:,2); BB = [BB;BB(1)];
   fill(p1(1,AA),p1(2,AA),'y'), hold on
   fill(p1(1,BB),p1(2,BB),'g'), hold on
   pause(0.05)
