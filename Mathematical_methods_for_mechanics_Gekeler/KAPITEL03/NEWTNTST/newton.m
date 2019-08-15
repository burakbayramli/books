function [X,newtonerror] = newton(fn,X0,TOL)
% Global Newton method after Hoellig
% INPUT: fn  Filename, fn(X,1) Function, fn(X,2) Gradient
%        X0  Start vector
%        TOL Estimated error <= TOL
% Output: X  Approximate solution of F(X) = 0 if exists

global d n                    % -------------------------------
newtonerror  = 0;
MAXITER = 100; MAXITERIM = 30;
DONE = 0; ITER = 0; LAMDA = 1; T = 0; X = X0;
while ~DONE
   ITER  = ITER + 1
   A     = feval(fn,X,2);         % Call gradient
   B     = feval(fn,X,1);         % Call function
   DIFF  = - A\B;
   DNORM = norm(DIFF);
   if T == 1
      LAMDA = min(1,2*LAMDA); T = 0;
   else
      T = 1;
   end
   Y     = X + LAMDA*DIFF;
   DONE1 = 0; ITERIM = 0;
   while ~DONE1
      ITERIM = ITERIM + 1;
      C      = feval(fn,Y,1);           % Call function
      DIFF1  = A\C;
      DNORM1 = norm(DIFF1);
      AUX    = (1 - LAMDA/2)*DNORM;
      DIFF2  = DNORM1 - AUX;
      if DIFF2 > TOL & ITERIM < MAXITERIM
         LAMDA = LAMDA/2; T = 0;
         Y     = Y - LAMDA*DIFF;
      else
         DONE1 = 1;
      end
      if ITERIM >= MAXITERIM, disp(' MAXITERIM attained '); newtonerror = 2
      end
   end
   if ITER >= MAXITER, disp(' MAXITER attained '); newtonerror = 1
   end
   X = Y;
   if DNORM < TOL | newtonerror ~= 0, DONE = 1; end
end
