function [XEND,PFAD] = simplex(PROBLEM,X1,Parmeter)
% minimizes a function by the method  of Nelder and Mead
% ohne Grafik
%%%%%%%%%%%%%%%%
X1 = X1';
%%%%%%%%%%%%%%%%%%%%%%%%
MAXITER = Parmeter(1); TOL = Parmeter(2); RR = Parmeter(3);
ALFA = 1; BETA = 0.5; GAMA = 2; DIFER = 0; NX = size(X1,2);
K1 = NX+1; K2 = NX+2; K3 = NX+3; K4 = NX+4;
ITER = 0;
X1 = [X1;zeros(3,NX)];
for I = 1:K1
   SUM(I) = feval(PROBLEM,X1(I,:));
end
DONE = 0; PFAD = X1(1,:);
while ~DONE
   SUMH = max(SUM(1:K1)); INDEX = max(find(SUM(1:K1) == SUMH));
   SUML = min(SUM(1:K1)); KOUNT = min(find(SUM(1:K1) == SUML));
   % find centroid of points with I different than index
   for J = 1:NX
      SUM2 = sum(X1(1:K1,J));
      X1(K2,J) = (SUM2 - X1(INDEX,J))/NX;
      X1(K3,J) = (1 + ALFA)*X1(K2,J) - ALFA*X1(INDEX,J);
   end
   SUM(K3) = feval(PROBLEM,X1(K3,:));
   if SUM(K3) < SUML
      %disp(' Expansion')
      X1(K4,:) = (1 - GAMA)*X1(K2,:) + GAMA*X1(K3,:);
      SUM(K4) = feval(PROBLEM,X1(K4,:));
      if SUM(K4) < SUML,
         X1(INDEX,:) = X1(K4,:);
      else
         X1(INDEX,:) = X1(K3,:);
      end
      SUM(INDEX) = feval(PROBLEM,X1(INDEX,:));
   else
      J = find([1:K1] ~= INDEX);
      SUMS = min(SUM(J));
      if SUM(K3) <= SUMS
         X1(INDEX,:) = X1(K3,:);
         SUM(INDEX) = feval(PROBLEM,X1(INDEX,:));
      else
         if SUM(K3) >= SUMH
            X1(INDEX,:) = X1(K3,:);
            SUM(INDEX) = feval(PROBLEM,X1(INDEX,:));
         end
        % disp(' Kontraktion')
         X1(K4,:) = BETA*X1(INDEX,:) + (1 - BETA)*X1(K2,:);
         SUM(K4) = feval(PROBLEM,X1(K4,:));
         if SUM(K4) <= SUMH
            X1(INDEX,:) = X1(K4,:);
            SUM(INDEX) = feval(PROBLEM,X1(INDEX,:));
         else
            X1(1:K1,:) = 0.5*(X1(1:K1,:) + ones(K1,1)*X1(KOUNT,:));
            for I = 1:K1
                SUM(I) = feval(PROBLEM,X1(I,:));
            end
         end
      end
   end
   SUM(K2) = feval(PROBLEM,X1(K2,:));
   AUX  = SUM(1:K1) - SUM(K2); DIFER = norm(AUX)/NX;
   ITER = ITER + 1;
   DONE = (DIFER < TOL) | (ITER >= MAXITER);
   PFAD = [PFAD;X1(K2,:)];
   ITER_DIFER = [ITER,DIFER]
end
XEND = X1(K2,:)';
PFAD = PFAD';
