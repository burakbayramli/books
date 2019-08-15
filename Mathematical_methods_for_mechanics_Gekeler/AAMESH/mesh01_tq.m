function [p1,e1,t1,q1] = mesh01_tq(FF2,p,e,t,q)
% gleichmaessige Verfeinerung
% von gleichzeitig auftretenden Dreiecken und konvexen Vierecken
% p muss 2 Zeilen haben
% t muss 3 Zeilen haben
% q muss 4 Zeilen haben
% e muss 5 Zeilen haben
% t oder q kann leer sein

p1 = p; e1 = e; t1 = t; q1 = q;  
L = size(p,2); M = L;
% -- Dreiecke ------------------------
if isempty(t)
   p_aux = p;
else
   N = size(t,2);
   pt  = []; t1 = [];
   for I = 1:N
      L1  = L+1; L2 = L+2; L3 = L+3;
      AUX = [t(1,I),     L1,     L3,  L1;
                 L1, t(2,I),     L2,  L2;
                 L3,     L2, t(3,I),  L3];
      t1  = [t1,AUX];
      J   = t(1:3,I);
      AUX = [(p(:,J(1))+p(:,J(2)))/2, ...
             (p(:,J(2))+p(:,J(3)))/2, ...
             (p(:,J(3))+p(:,J(1)))/2];
      pt  = [pt,AUX];
      L   = L + 3;
   end
   % -- doppelte Knoten eliminieren ------
   %p_aux = [p, pt];
   %[p_aux,t1] = mesh04(p_aux,t1);
   [M1,N1] = size(t1);
   [pt,I,J] = unique(pt','rows');
   J = [[1:M],M+J']; C = t1(:); D = J(C);
   t1 = reshape(D,M1,N1); pt = pt';
   p_aux = [p,pt];
end
% -- Vierecke -------------------------
if ~isempty(q)
   L = size(p_aux,2); N =  size(q,2);
   pq = []; q1 = [];
   for I = 1:N
      L1 = L+1; L2 = L+2; L3 = L+3; L4 = L+4; L5 = L+5;
      AUX = [q(1,I),     L1,     L5,    L4;
                 L1, q(2,I),     L2,    L5;
                 L5,     L2, q(3,I),    L3;
                 L4,     L5,     L3, q(4,I)];
      q1 = [q1,AUX];
      J = q(1:4,I);
      AUX = [(p(:,J(1)) + p(:,J(2)))/2, ...
             (p(:,J(2)) + p(:,J(3)))/2, ...
             (p(:,J(3)) + p(:,J(4)))/2, ...
             (p(:,J(4)) + p(:,J(1)))/2, ...
             (p(:,J(1)) + p(:,J(2))...
              + p(:,J(3))+ p(:,J(4)))/4];
      pq = [pq,AUX];
      L = L + 5;
   end
   % -- doppelte Knoten eliminieren ----------
   p1 = [p_aux,pq]; 
   [p1,q1] = mesh04(p1,q1);  % wg. Ordnung !
end
% -- Rand verfeinern ----------------------
e1 = [];
for I = 1:size(e,2)
   X  = p(1,e(1:2,I)); Y = p(2,e(1:2,I));
   ZWP = [(X(1) + X(2))/2;(Y(1)+Y(2))/2];
   DIFF = p1 - ZWP*ones(1,size(p1,2));
   NORMDIFF = sqrt(DIFF(1,:).*DIFF(1,:) + DIFF(2,:).*DIFF(2,:));
   J = min(find(NORMDIFF < 100*eps));
   E12 = [[e(1,I);J],[J;e(2,I)]];
   E34 = [[e(3,I); (e(3,I)+ e(4,I))/2],[(e(3,I)+ e(4,I))/2; e(4,I)]];
   E5  = [e(5,I),e(5,I)]; 
   AUX = [E12;E34;E5];
   e1 = [e1,AUX];
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