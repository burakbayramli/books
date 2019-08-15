function [p1,e1,t1] = mesh01_t(FF2,p,e,t,it)
% E. Gekeler, Universitaet Stuttgart, Release 19.1.06
% gleichmaessige Verfeinerung von Dreiecken
% Falls Rand stueckweise gerade, dann kann FF2 = [] gesetzt werden
% p muss 2 Zeilen haben
% t muss 3 Zeilen haben
% e muss 5 Zeilen haben
% Bei nargin = 5 werden nur die Dreiecke mit 
% Dreiecksnrn. in it  verfeinert
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
   t1 = [t(:,MM),t1];
end
% -- haengende Knoten beseitigen ----------------------
if nargin == 5
   t1 = mesh50(p1,t1);
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