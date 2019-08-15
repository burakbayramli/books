function [W,errorcode] = desc(name,X,TOL);
% Verfahren des steilsten Abstiegs
% mit GA_Test
% errorcode = 1: Abstiegsrichtung unzulaessig
% errorcode = 2: GA_Test versagt
% errorcode = 1: Max. Scrittzahl in Iteration

MAXITER   = 20;
errorcode = 0;
A         = eye(length(X));
W         = X;
ITER      = 0;
DONE      = 0;
while ~DONE
   ITER   = ITER + 1;
   GRAD   = feval(name,X,2);
   D      = GRAD';
   [Y,errorcode] = ga_test(name,GRAD,X,D,1);
   DIFF   = X - Y;
   W      = [W,Y];
   X      = Y;
   NORM   = norm(GRAD);
   DONE   = norm(DIFF) < TOL | ITER > MAXITER | NORM < TOL;
end
if ITER > MAXITER
   disp(' Steepest-Descent versagt! ');
   errorcode = 3;
end
