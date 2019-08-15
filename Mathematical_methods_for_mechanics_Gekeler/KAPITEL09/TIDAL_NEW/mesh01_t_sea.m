function [p1,e1,t1,u1] = mesh01_t_sea(FF2,p,e,t,u)
% E. Gekeler, Universitaet Stuttgart, Release 18.9.09
% uniform refinement of triangles
% if boundary piecewise linear, one may put FF2 = []
% p must have 2 rows
% t must have 4 rows
% e must have seven rows
p1 = p; e1 = e; t1 = t; tt = t;
e1 = e; L = size(p,2); M = L;
if ~isempty(t)
   N = size(tt,2);  pt  = []; t1 = []; row4 = []; ut = [];
   for I = 1:N
      L1 = L+1; L2 = L+2; L3 = L+3;
      AUX1 = [tt(1,I),      L1,      L3,  L1;
                  L1, tt(2,I),      L2,  L2;
                  L3,      L2, tt(3,I),  L3];
      t1 = [t1,AUX1]; 
      row4 = [row4,tt(4,I)*ones(1,4)];
      J  = tt(1:3,I);
      AUX  = [(p(:,J(1))+p(:,J(2)))/2, ...
              (p(:,J(2))+p(:,J(3)))/2, ...
              (p(:,J(3))+p(:,J(1)))/2];
      pt = [pt,AUX];
      AUX  = [(u(J(1))+u(J(2)))/2, ...
              (u(J(2))+u(J(3)))/2, ...
              (u(J(3))+u(J(1)))/2];
      ut = [ut,AUX];
      L  = L + 3;
   end
end
% -- elimination of double nodes ----------
% -- Sequence of noces in p must not be changed because of
% -- edge matrix e

[M1,N1] = size(t1);
[pt,I,J] = unique(pt.','rows');
J = [[1:M],M+J.']; C = t1(:); D = J(C);
t1 = reshape(D,M1,N1); pt = pt.'; p1 = [p,pt];
ut = ut(I);
u1 = [u,ut];
t1 = [t1;row4];  
% -- refinement of boundary ----------------------
e1 = []; 
for I = 1:size(e,2)
   X  = p(1,e(1:2,I)); Y = p(2,e(1:2,I));
   ZWP = [(X(1) + X(2))/2;(Y(1)+Y(2))/2];
   DIFF = pt - ZWP*ones(1,size(pt,2));
   NORMDIFF = sqrt(DIFF(1,:).*DIFF(1,:) + DIFF(2,:).*DIFF(2,:));
   J = find(NORMDIFF < 100*eps);
   if length(J) > 1, disp(' Geometry error! ')
      return
   end
   if isempty(J)
      e1 = [e1,e(:,I)];
   else 
      J = J + M;
      E12 = [[e(1,I);J],[J;e(2,I)]];
      E34 = [[e(3,I); (e(3,I)+ e(4,I))/2],[(e(3,I)+ e(4,I))/2; e(4,I)]];
      E5  = [e(5:7,I),e(5:7,I)]; 
      AUX = [E12;E34;E5];
      e1 = [e1,AUX];
   end
end
% -- replacing of nodes in curved boundary -------------
if ~isempty(FF2)
   SEGZAHL = max(e1(5,:)); SEGZAHL = SEGZAHL(1);
   for I = 1:SEGZAHL
      J = find(e1(5,:) == I); SEGRAND = e1(3,J);
      [X,Y] = feval(FF2,I,SEGRAND);
      p1(1,e1(1,J)) = X; p1(2,e1(1,J)) = Y;
   end
end
