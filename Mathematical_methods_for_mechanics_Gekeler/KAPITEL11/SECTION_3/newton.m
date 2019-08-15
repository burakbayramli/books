function [X,errorcode] = newton(Name,X0,Parmtr1,Parmtr2)
% Globales Newton-Verfahren
% mit PINV.M
% INPUT: NAME1: Verfahren: Funktion F bzw. Gradient FD aufrufen
%        Name2: Beispiel
%        X0   Startvektor
%        TOL  Geschaetzter Fehler <= TOL
% Output: Y   Loesung von F(X) = 0, falls sie existiert
% ACHTUNG: Abbruchkriterium bedeutet nicht Konvergenz!!!

F          = [Name];
errorcode  = 0;
Maxit      = Parmtr1(1);
TOL        = Parmtr1(2);
TOL_I      = Parmtr1(3);
n          = Parmtr1(4);
MAXITERIM  = 15;   % Max. Anzahl Lambda-Halbierung

DONE       = 0;
ITER       = 0;
LAMBDA     = 1;
TT         = 0;
X          = X0;
DIFFNORM   =  [];
while ~DONE
   ITER      = ITER + 1;
   A         = feval(F,X,2,Parmtr2);
   B         = - feval(F,X,1,Parmtr2);
   DIFF      = A\B;
   DNORM     = norm(DIFF)/n;
   if TT == 1
      LAMBDA =  min(1,2*LAMBDA);
      TT      = 0;
   else
      TT      = 1;
   end
   Y         = X +  LAMBDA*DIFF;
   DONE1     = 0;
   ITERIM    = 0;
   while ~DONE1
      ITERIM = ITERIM + 1;
      C      = feval(F,Y,1,Parmtr2);
      DIFF1 = A\C;
      DNORM1 = norm(DIFF1)/n;
      AUX    = (1 - LAMBDA/2)*DNORM;
      DIFF2  = DNORM1 - AUX;
      if DIFF2 > TOL_I & ITERIM < MAXITERIM
         TT      = 0;
         LAMBDA = LAMBDA/2;
         Y      = Y - LAMBDA*DIFF;
      else
         DONE1  = 1;
      end
      if ITERIM >= MAXITERIM;
         disp(' MAXITERIM erreicht ');
         errorcode = 2
      end
   end
   if ITER >= Maxit
      disp(' Maxit erreicht ');
      errorcode = 1
   end
   DIFFERENZ = Y - X;
   DNORM3    = norm(DIFFERENZ,inf);
   X         = Y;
   if DNORM3 < TOL | errorcode ~= 0
      DONE   = 1;
   end
   ITER_DNORM3_ITERIM = [ITER,DNORM3,ITERIM]
   DIFFNORM = [DIFFNORM; DNORM3];
end
%DIFFNORM
