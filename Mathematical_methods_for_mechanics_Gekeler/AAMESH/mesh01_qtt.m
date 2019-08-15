function [p1,e1,t1,q1] = mesh01_qtt(FF2,p,e,t,q,it)
% Eckart Gekeler, Universitaet Stuttgart, Release 20.01.06
% Verfeinerung von Dreiecken
% bei ev. gleichzeitig auftretenden Vierecken
% p muss 2 Zeilen haben
% t muss 3 Zeilen haben
% q muss 4 Zeilen haben
% e muss 5 Zeilen haben
% Bei nargin = 5 werden nur die Dreiecke mit 
% Dreiecksnrn. in it  verfeinert

GRAFIK = 0;
p1 = p; e1 = e; t1 = t; q1 = q;  
p1 = p; e1 = e; t1 = t; q1 = q;
if nargin == 5 tt = t; end
if nargin == 6, tt = t(:,it); end
if nargin == 6 & isempty(it), return; end

L = size(p,2); M = L;
if ~isempty(t)
   L = size(p,2); N = size(tt,2);
   pt  = []; t1 = [];
   for I = 1:N
      L1  = L+1; L2 = L+2; L3 = L+3;
      AUX = [tt(1,I),     L1,     L3,  L1;
                 L1, tt(2,I),     L2,  L2;
                 L3,     L2, tt(3,I),  L3];
      t1  = [t1,AUX];
      J   = tt(1:3,I);
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
pt = pt';
p1 = [p,pt];
J = [[1:M],M+J']; C = t1(:); D = J(C);
t1 = reshape(D,M1,N1); 
if ~isempty(it), MM = setxor([1:size(t,2)],it);
   if GRAFIK == 1
      for J = 1:size(t1,2)
         XA = p1(1,t1(:,J)); YA = p1(2,t1(:,J));
         fill(XA,YA,'g'), hold on
         pause
      end
   end
   t1 = [t(:,MM),t1];
end
end
% -- Rand verfeinern ----------------------
e1 = [];
for I = 1:size(e,2)
   X  = p(1,e(1:2,I)); Y = p(2,e(1:2,I));
   ZWP = [(X(1) + X(2))/2;(Y(1)+Y(2))/2];
   DIFF = pt - ZWP*ones(1,size(pt,2));
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
      J = J + M;
      E12 = [[e(1,I);J],[J;e(2,I)]];
      E34 = [[e(3,I); (e(3,I)+ e(4,I))/2],[(e(3,I)+ e(4,I))/2; e(4,I)]];
      E5  = [e(5,I),e(5,I)]; 
      AUX = [E12;E34;E5];
      e1 = [e1,AUX];
   end
end
% -- haengende Knoten beseitigen ----------------------
if ~isempty(it)
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