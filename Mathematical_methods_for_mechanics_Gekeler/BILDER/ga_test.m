function [Y,errorcode] = ga_test(name,GRAD,X,D,TOL)
% GA_Test,  vgl. Spellucci, S. 99
% errorcode = 1: Abstiegsrichtung unzulaessig
% errorcode = 2: Max. Schrittzahl erreicht

MAXITER   = 20;
errorcode = 0;
alpha     = 0.5;
delta     = 0.05;
ITER      = 0;
F         = feval(name,X,1); % Funktion
if GRAD*D < 0
  disp(' Abstiegsrichtung unzulaessig! ');
  errorcode = 1;
end
% --------------------
D = 2*D;
% ---------------------
Y         = X - D;
F1        = feval(name,Y,1);
R         = delta*GRAD*D;
DONE      =  F1 < F - R & errorcode == 0;
while ~DONE
   ITER   = ITER + 1;
   D      = alpha*D;
   R      = alpha*R;
   Y      = X - D;
   F1     = feval(name,Y,1);
   DONE   = (F1 < F-R | ITER > MAXITER);
end
if ITER > MAXITER
   disp(' GA_Test versagt! ')
   errorcode = 2;
end
