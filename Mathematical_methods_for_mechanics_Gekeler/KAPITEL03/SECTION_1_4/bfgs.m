function [W,errorcode] = bfgs(name,X,TOL);
% Eckart Gekeler, Universitaet Stuttgart, Release 17.4.05
% BFGS method, cf. Spellucci, S. 135
% INPUT: name Name of function, e.g. 'fn'
%        X    Start vector
%        TOL  Tolerance
% errorcode = 1: descend direction unfeasible
% errorcode = 2: GA_Test fails
% errorcode = 3: Max. step number in iteration

MAXITER  = 10; errorcode = 0;
A = eye(length(X)); W = X; ITER = 0; DONE = 0;
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
if ITER > MAXITER
   disp(' Bad or no solution! ');
   errorcode = 3;
end
