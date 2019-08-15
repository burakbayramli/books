function [X,errorcode] = newton(F,X0,TOL,MAXIT,Parmeter)
% Quasi-global Newton method
% INPUT: NAME  Call Function F resp. Gradient FD 
%        X0   start vector
%        TOL  estimated error <= TOL
% Output: Y   solution of F(X) = 0, if exists

errorcode = 0; MAXITERIM = 30;
DONE = 0; ITER = 0; LAMBDA = 1; T = 0;
X = X0;
while ~DONE
   ITER  = ITER + 1;
   A     = feval(F,X,2,Parmeter);   % Gradient aufrufen!
   %KOND = condest(A);
   B     = feval(F,X,1,Parmeter); % Funktion aufrufen!
   DIFF  = - A\B;
   % -------------------------
   % clf, plot(DIFF), pause
   % -------------------------
   DNORM = norm(DIFF);
   if T == 1
      LAMBDA =  min(1,2*LAMBDA); T = 0;
   else
      T = 1;
   end
   Y      = X +  LAMBDA*DIFF;
   DONE1  = 0;
   ITERIM = 0;
   while ~DONE1
      ITERIM = ITERIM + 1;
      C      = feval(F,Y,1,Parmeter); % Funktion aufrufen!
      DIFF1  = A\C;
      DNORM1 = norm(DIFF1);
      AUX    = (1 - LAMBDA/2)*DNORM;
      DIFF2  = DNORM1 - AUX;
      if DIFF2 > TOL & ITERIM < MAXITERIM
         T = 0;
         LAMBDA = LAMBDA/2;
         Y = Y - LAMBDA*DIFF;
      else
         DONE1 = 1;
      end
      if ITERIM >= MAXITERIM
         disp(' MAXITERIM erreicht ');
         errorcode = 2
      end
   end
   if ITER >= MAXIT
      disp(' MAXITER erreicht ');
      errorcode = 1;
   end
   X = Y;
   if DNORM < TOL | errorcode ~= 0
      DONE = 1;
   end
   IT_ITERIM_DNORM = [ITER,ITERIM,DNORM];
   %KOND
end
