function [X,newtonerror] = newton(fn,X0,MAXITER,TOL,Parmeter)
% Globales Newton-Verfahren nach Hoellig
% INPUT: fn  Filename, fn(X,1) Funktion, fn(X,2) Gradient
%        X0  Startvektor
%        TOL Geschaetzter Fehler <= TOL
% Output: X  Loesung von F(X) = 0, falls existiert

newtonerror  = 0;
MAXITERIM = 30;
DONE = 0; ITER = 0; LAMDA = 1; T = 0; X = X0;
while ~DONE
   ITER  = ITER + 1;
   A     = feval(fn,X,2,Parmeter);         % Gradient aufrufen!
   B     = feval(fn,X,1,Parmeter);         % Funktion aufrufen!
   
   DIFF  = - A\B;
   DNORM = norm(DIFF,inf);
   if T == 1
      LAMDA = min(1,2*LAMDA); T = 0;
   else
      T = 1;
   end
   Y     = X + LAMDA*DIFF;
   DONE1 = 0; ITERIM = 0;
   while ~DONE1
      ITERIM = ITERIM + 1;
      C      = feval(fn,Y,1,Parmeter);           % Funktion aufrufen!
      DIFF1  = A\C;
      DNORM1 = norm(DIFF1,inf);
      AUX    = (1 - LAMDA/2)*DNORM;
      DIFF2  = DNORM1 - AUX;
      if DIFF2 > TOL & ITERIM < MAXITERIM
         LAMDA = LAMDA/2; T = 0;
         Y     = Y - LAMDA*DIFF;
      else
         DONE1 = 1;
      end
      if ITERIM >= MAXITERIM, disp(' MAXITERIM erreicht '); newtonerror = 2;
      end
   end
   if ITER >= MAXITER, disp(' MAXITER erreicht '); newtonerror = 1;
   end
   X = Y;
   if DNORM < TOL | newtonerror ~= 0, DONE = 1; end
   ITER_ITERIM_DNORM = [ITER,ITERIM,DNORM]
end
