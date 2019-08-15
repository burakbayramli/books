function [X,errorcode] = newton(F,X0,J,TOL,Parmeter)
% Global Newton method for PITCON
% INPUT: NAME Function F resp. Gradient FD 
%        X0   Start vector
%        TOL  Estimated error <= TOL
% Output: Y   Solution of F(X) = 0, if exists

global d n                    
errorcode = 0;
MAXITER   = 100;
MAXITERIM = 30;
DONE      = 0;
ITER      = 0;
LAMBDA    = 1;
T         = 0;
X         = X0;
while ~DONE
   ITER = ITER + 1;
   A    = feval(F,X,2,Parmeter);   % Call Gradient!
   % -- Modifiaction of gradient for PITCON ------------
   E    = zeros(1,length(X));
   E(J) = 1;
   A    = [A;E];
   % ------------------------------------------------------
   B = feval(F,X,1,Parmeter); % Funktion aufrufen!
   % -- Modification of function for PITCON ---------------
   B = [B; X(J) - X0(J)];
   % ------------------------------------------------------
   DIFF  = - A\B;
   DNORM = norm(DIFF);
   if T == 1
      LAMBDA =  min(1,2*LAMBDA);
      T = 0;
   else
      T = 1;
   end
   Y     = X +  LAMBDA*DIFF;
   DONE1 = 0;
   ITERIM    = 0;
   while ~DONE1
      ITERIM = ITERIM + 1;
      C = feval(F,Y,1,Parmeter); % Funktion aufrufen!
      % -- Abaenderung der Funktion fuer PITCON -----------
      C = [C; X(J) - X0(J)];
      % --------------------------------------------------
      DIFF1  = A\C;
      DNORM1 = norm(DIFF1);
      AUX    = (1 - LAMBDA/2)*DNORM;
      DIFF2  = DNORM1 - AUX;
      if DIFF2 > TOL & ITERIM < MAXITERIM
         T      = 0;
         LAMBDA = LAMBDA/2;
         Y      = Y - LAMBDA*DIFF;
      else
         DONE1  = 1;
      end
      if ITERIM >= MAXITERIM
         disp(' MAXITERIM attained ');
         errorcode = 2
      end
   end
   if ITER >= MAXITER
      disp(' MAXITER attained ');
      errorcode = 1;
   end
   X = Y;
   if DNORM < TOL | errorcode ~= 0
      DONE = 1;
   end
end
