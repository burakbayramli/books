function KNOTEN2 = mesh14(p,MAXIT);
% Eckart Gekeler, Universitaet Stuttgart, Release 8.4.05
% Gebietszerlegung durch Offsetting der Normalen
% TODO!
N = size(p,2);
KNOTEN2 = [p;ones(1,N)]; RAND = p;
AUX = [p(:,2:N),p(:,1)]; TAN = AUX - p;
LN  = sqrt(TAN(1,:).*TAN(1,:) + TAN(2,:).*TAN(2,:));
MAX = max(LN); MIN = min(LN);
DONE = 0; ITER = 0;
while ~DONE
   ITER
   ITER = ITER + 1;
   N = size(RAND,2);
   if N > 3 & ITER <= MAXIT
      [KNOTEN,KNOTEN1]  = mesh15(RAND,MAX,MIN);
      if ~isempty(KNOTEN)
         KNOTEN2 = [KNOTEN2,[KNOTEN;(ITER + 1)*ones(1,size(KNOTEN,2))]];
      end
      if ~isempty(KNOTEN1)
         KNOTEN2 = [KNOTEN2,[KNOTEN1;zeros(1,size(KNOTEN1,2))]];
      end
      RAND = KNOTEN;
   else
      DONE = 1;
   end
end
