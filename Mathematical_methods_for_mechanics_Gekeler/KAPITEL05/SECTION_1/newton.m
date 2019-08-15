function [X,errorcode] = newton(NAME,X0,TOL,MAXIT,Parmeter1,Parmeter2)
% Global Newton method
% slight modification for bifurcation
% INPUT: NAME Funktion F bzw. Gradient FD aufrufen
%        X0   Startvektor
%        TOL  Geschaetzter Fehler <= TOL
% Output: Y   Loesung von F(X) = 0, falls existiert

F = [NAME]; MAXITERIM = 30; 
errorcode = 0; DONE = 0; ITER = 0; LAMBDA = 1;
T = 0; X  = X0;
while ~DONE
   ITER  = ITER + 1;
   A     = feval(F,X,2,Parmeter1,Parmeter2);   % Gradient aufrufen!
   %KOND = condest(A);
   B     = feval(F,X,1,Parmeter1,Parmeter2); % Funktion aufrufen!
   DIFF  = - A\B;
   % -------------------------
   % clf, plot(DIFF), pause
   % -------------------------
   DNORM = norm(DIFF);
   if T == 1, LAMBDA = min(1,2*LAMBDA); T = 0;
   else, T = 1;
   end
   Y = X + LAMBDA*DIFF; DONE1 = 0; ITERIM = 0;
   while ~DONE1
      ITERIM = ITERIM + 1;
      C      = feval(F,Y,1,Parmeter1,Parmeter2); % Funktion aufrufen!
      DIFF1  = A\C;
      DNORM1 = norm(DIFF1);
      AUX    = (1 - LAMBDA/2)*DNORM;
      DIFF2  = DNORM1 - AUX;
      if DIFF2 > TOL & ITERIM < MAXITERIM
         T = 0; LAMBDA = LAMBDA/2; Y = Y - LAMBDA*DIFF;
      else
         DONE1 = 1;
      end
      if ITERIM >= MAXITERIM
         disp(' MAXITERIM erreicht '); errorcode = 2
      end
   end
   if ITER >= MAXIT
      disp(' MAXITER erreicht ');
      errorcode = 1;
   end
   X = Y;
   if DNORM < TOL | errorcode ~= 0, DONE = 1; end
   RES = feval(F,Y,1,Parmeter1,Parmeter2); % Funktion aufrufen!
   IT_ITERIM_DNORM_RESIDUUM = [ITER,ITERIM,DNORM,norm(RES)]
   %KOND
end
