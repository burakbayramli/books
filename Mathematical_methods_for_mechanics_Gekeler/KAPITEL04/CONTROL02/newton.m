function [X,errorcode] = newton(F,X0,G,Parmeter1,Parmeter2,Parmeter3)
% Globalized Newton method
% INPUT: F   Function F resp. Gradient FD
%        X0  Start vector
%        TOL Estimated error <= TOL
% Output: Y  Solution of F(X) = 0, if exists

errorcode  = 0; Maxit = Parmeter1(1);
TOL = Parmeter1(2);
MAXITERIM  = 30;   % Max. number Lambda-Halving

DONE = 0; ITER = 0; LAMBDA = 1; TT = 0; X = X0;
DIFFNORM = [];
while ~DONE
   ITER  = ITER + 1;
   A     = feval(F,X,2,G,Parmeter2,Parmeter3);
   B     = - feval(F,X,1,G,Parmeter2,Parmeter3);
   DIFF  = A\B;
   DNORM = norm(DIFF,inf);
   if TT == 1, LAMBDA =  min(1,2*LAMBDA); TT = 0;
   else, TT = 1;
   end
   Y = X +  LAMBDA*DIFF;
   DONE1 = 0; ITERIM = 0;
   while ~DONE1
      ITERIM = ITERIM + 1;
      C      = feval(F,Y,1,G,Parmeter2,Parmeter3);
      DIFF1  = A\C;
      DNORM1 = norm(DIFF1,inf);
      AUX    = (1 - LAMBDA/2)*DNORM;
      DIFF2  = DNORM1 - AUX;
      if DIFF2 > TOL & ITERIM < MAXITERIM
         TT = 0; LAMBDA = LAMBDA/2;
         Y  = Y - LAMBDA*DIFF;
      else
         DONE1 = 1;
      end
      if ITERIM >= MAXITERIM;
         disp(' MAXITERIM erreicht '); errorcode = 2
      end
   end
   if ITER >= Maxit, disp(' Maxit erreicht '); errorcode = 1
   end
   X = Y;
   if DNORM < TOL | errorcode ~= 0, DONE = 1; end
 % ITERIM
   DIFFNORM = [DIFFNORM; DNORM];
end
%DIFFNORM
