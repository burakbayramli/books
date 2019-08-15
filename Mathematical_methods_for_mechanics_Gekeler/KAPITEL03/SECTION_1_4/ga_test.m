function [Y,errorcode] = ga_test(name,GRAD,X,D,TOL)
% Eckart Gekeler, Universitaet Stuttgart, Release 17.4.05
% GA-test,  cf. Spellucci, p. 99
% errorcode = 1: Descend direction unfeasible
% errorcode = 2: Max. step number

MAXITER   = 20; errorcode = 0; ITER = 0;
alpha     = 0.5; delta     = 0.05;
F         = feval(name,X,1); % Funktion
if GRAD*D < 0
  disp(' Descend direction unfeasible! ');
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
   disp(' GA-Test fails! ')
   errorcode = 2;
end
