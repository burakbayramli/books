function [p1,e1,t1,q1] = mesh01_tqq(FF2,p,e,t,q,iq)
% Eckart Gekeler, Universitaet Stuttgart, Release 20.01.06
% Verfeinerung von Vierecken
% bei ev. gleichzeitig auftretenden Dreiecken
% p muss 2 Zeilen haben
% t muss 3 Zeilen haben
% q muss 4 Zeilen haben
% e muss 5 Zeilen haben
% Bei nargin = 5 werden nur die Vierecke mit 
% Vierecksnrn. in iq  verfeinert

GRAFIK = 0;
p1 = p; e1 = e; t1 = t; q1 = q;  
p1 = p; e1 = e; t1 = t; q1 = q;
if nargin == 5 qq = q; end
if nargin == 6, qq = q(:,iq); end
if nargin == 6 & isempty(iq), return; end

L = size(p,2); M = L;
if ~isempty(q)
   N =  size(qq,2);
   pq = []; q1 = [];
   for I = 1:N
      L1 = L+1; L2 = L+2; L3 = L+3; L4 = L+4; L5 = L+5;
      AUX = [qq(1,I),     L1,     L5,    L4;
                 L1, qq(2,I),     L2,    L5;
                 L5,     L2, qq(3,I),    L3;
                 L4,     L5,     L3, qq(4,I)];
      q1 = [q1,AUX];
      J = qq(1:4,I);
      AUX = [(p(:,J(1)) + p(:,J(2)))/2, ...
             (p(:,J(2)) + p(:,J(3)))/2, ...
             (p(:,J(3)) + p(:,J(4)))/2, ...
             (p(:,J(4)) + p(:,J(1)))/2, ...
             (p(:,J(1)) + p(:,J(2))...
              + p(:,J(3))+ p(:,J(4)))/4];
      pq = [pq,AUX];
      L = L + 5;
   end
end
% -- doppelte Knoten eliminieren ------
%p_aux = [p, pq];
%[p_aux,q1] = mesh04(p_aux,q1);
[M1,N1] = size(q1);
[pq,I,J] = unique(pq','rows');
pq = pq';
p1 = [p,pq];
J = [[1:M],M+J']; C = q1(:); D = J(C);
q1 = reshape(D,M1,N1); 
if ~isempty(iq), MM = setxor([1:size(q,2)],iq);
   if GRAFIK == 1
      for J = 1:size(q1,2)
         XA = p1(1,q1(:,J)); YA = p1(2,q1(:,J));
         fill(XA,YA,'g'), hold on
         pause
      end
   end
   q1 = [q(:,MM),q1];
end

% -- Rand verfeinern ----------------------
e1 = [];
for I = 1:size(e,2)
   X  = p(1,e(1:2,I)); Y = p(2,e(1:2,I));
   ZWP = [(X(1) + X(2))/2;(Y(1)+Y(2))/2];
   DIFF = pq - ZWP*ones(1,size(pq,2));
   AUX = DIFF(1,:).*DIFF(1,:) + DIFF(2,:).*DIFF(2,:);
   AUX1 = min(AUX);
   J = min(find(AUX == AUX1));
   Q = (AUX1 < 100*eps);
   if length(J) > 1, disp(' Geometriefehler! ')
      return
   end
   if Q == 0
      e1 = [e1,e(:,I)];
   else 
      J = J+M;
      E12 = [[e(1,I);J],[J;e(2,I)]];
      E34 = [[e(3,I); (e(3,I)+ e(4,I))/2],[(e(3,I)+ e(4,I))/2; e(4,I)]];
      E5  = [e(5,I),e(5,I)]; 
      AUX = [E12;E34;E5];
      e1 = [e1,AUX];
   end
end
% -- haengende Knoten beseitigen ----------------------
if ~isempty(iq)
   [p1,t1,q1] = mesh51(p1,t1,q1);
   t1 = mesh50(p1,t1);
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