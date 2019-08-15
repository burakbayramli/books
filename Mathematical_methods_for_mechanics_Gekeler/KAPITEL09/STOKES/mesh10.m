function p1 = mesh10(p,e,t,n);
% for triangular meshes
% moves nodes into center of neighboring polygon
% running in forward and backward numeration 
% n number of runnings total 

if nargin == 3, Maxit = 2; else Maxit = n; end
M = size(p,1); N = size(t,2);
paux  = [p(1:2,:); zeros(1,size(p,2))];
paux(3,e(1,:)) = 1;
INNERPKTE  = find(paux(3,:) == 0);
paux = paux(1:2,:);
for ITER = 1:Maxit
   for K = 1:length(INNERPKTE)
      taux = [t(1:3,:); ones(1,N)];
      for L = 1:N
         if any(taux(1:3,L) == INNERPKTE(K))
            taux(4,L) = 0;
         end
      end
      J = find(taux(4,:) == 0);
      NEIGHBOR = [];
      for L = 1:length(J)
         NEIGHBOR = [NEIGHBOR; taux(1:3,J(L))];
      end
      NEIGHBOR = unique(NEIGHBOR);
      P  = length(NEIGHBOR);
      if P >= 3 
         paux(1:2,INNERPKTE(K)) = ...
         [sum(paux(1,NEIGHBOR))/P; sum(paux(2,NEIGHBOR))/P];
      end
   end
   INNERPKTE = fliplr(INNERPKTE);
end
if M == 2, p1 = paux; else, p1 = [paux;p(3:end,:)]; end
