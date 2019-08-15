function [W,errorcode] = bfgs(name,X,TOL);
% BFGS-Verfahren, vgl. Spellucci, S. 135
% INPUT: name Name der Funktion, z.B. 'fn'
%        X    Startvektor
%        TOL  Toleranz
% errorcode = 1: Abstiegsrichtung unzulaessig
% errorcode = 2: GA_Test versagt
% errorcode = 1: Max. Scrittzahl in Iteration

MAXITER  = 10; errorcode = 0;
A        = eye(length(X));
W        = X; ITER = 0; DONE = 0;
GRAD     = feval(name,X,2);
while ~DONE
   ITER  = ITER + 1;
   D     = A\GRAD';
   [Y,errorcode] = ga_test(name,GRAD,X,D,1);
   U     = Y - X;
   V     = A*U;
   GRAD1 = feval(name,Y,2);
   NORM  = norm(GRAD1);
   Z     = GRAD1 - GRAD;
   A     =  A  - V*V'/(V'*U) + Z'*Z/(Z*U);
   GRAD  = GRAD1;
   X     = Y;
   W     = [W,X];
   DONE  = norm(U) < TOL | ITER > MAXITER | NORM < TOL;
end
if ITER > MAXITER  errorcode = 3; end
