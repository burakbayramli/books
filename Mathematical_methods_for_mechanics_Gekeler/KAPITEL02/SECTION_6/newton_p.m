function [X,errorcode] = newton_p(FCN,X0,Name2,Parmeter1,Parmeter2,Parmeter3)
% Eckart Gekeler, Universitaet Stuttgart, Release 8.4.05
% Quasi-global Newton method
% INPUT: NAME1: Method: Calls Function F resp. Gradient FD
%        Name2: Example 
%        X0   Initial vector
%        TOL  estimated error <= TOL
% Output: Y   Solution of F(X) = 0, if exists
% ATTENTION:  Success does not mean convergence!!!
errorcode  = 0;
Maxit = Parmeter1(1); TOL = Parmeter1(2); TOL_I = Parmeter1(3);
MAXITERIM  = 30;   % Max. number of Lambda-halving
DONE = 0; ITER = 0; LAMBDA = 1; TT = 0; X = X0;
DIFFNORM   =  [];
while ~DONE
   ITER  = ITER + 1;
   [A,V] = feval(FCN,X,2,Name2,Parmeter2,Parmeter3);
   [B,V] = feval(FCN,X,1,Name2,Parmeter2,Parmeter3);
 %  Parmeter2 = [Parmeter2(1:2);V];
   DIFF  = - mpsolv(A,B);
   DNORM = norm(DIFF,inf);
   if TT == 1, LAMBDA =  min(1,2*LAMBDA); TT = 0;
   else, TT = 1;
   end
   Y      = X +  LAMBDA*DIFF;
   DONE1  = 0;
   ITERIM = 0;
   while ~DONE1
      ITERIM = ITERIM + 1;
      [C,V]      = feval(FCN,Y,1,Name2,Parmeter2,Parmeter3);
%     Parmeter2  = [Parmeter2(1:2);V];
      DIFF1  = mpsolv(A,C);
      DNORM1 = norm(DIFF1,inf);
      AUX    = (1 - LAMBDA/2)*DNORM;
      DIFF2  = DNORM1 - AUX;
      if DIFF2 > TOL_I & ITERIM < MAXITERIM
         TT = 0; LAMBDA = LAMBDA/2; Y = Y - LAMBDA*DIFF;
      else, DONE1  = 1;
      end
      if ITERIM >= MAXITERIM;
         disp(' MAXITERIM attained '); errorcode = 2
      end
   end
   if ITER >= Maxit, disp(' Maxit attained '); errorcode = 1
   end
   DIFFERENZ = Y - X;
   DNORM3 = norm(DIFFERENZ,inf);
   X = Y;
   if DNORM3 < TOL | errorcode ~= 0
      DONE = 1;
   end
   ITER_DNORM3_ITERIM = [ITER,DNORM3,ITERIM]
   DIFFNORM = [DIFFNORM; DNORM3];
   Parmeter2  = [Parmeter2(1:2);V];
end
DIFFNORM;
