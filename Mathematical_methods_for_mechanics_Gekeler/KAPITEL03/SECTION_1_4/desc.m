function [W,errorcode] = desc(name,X,TOL);
% Eckart Gekeler, Universitaet Stuttgart, Release 17.4.05
% Method of steepest descend with GA-test
% errorcode = 1: Descend direction unfeasible
% errorcode = 2: GA-test fails
% errorcode = 3: Max. Step number in iteration

MAXITER = 20; errorcode = 0; A = eye(length(X));
W = X; ITER = 0; DONE = 0;
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
   disp(' Bad or no solution! ');
   errorcode = 3;
end
