function [T,X,U,errorcode] = grad02(name,X0,YT,tol)
% Eckart Gekeler, Universitaet Stuttgart, Release 17.4.05
% Gradientenverfahren fuer Kontrollprobleme
% eine zusaetzliche Endbedingung

global d n t0 t1 U X Y
F = [name '_f']; G = [name '_g']; H = [name '_h'];
errorcode = 0; epsilon   = 1;
MAXITER   = 20;
%MAXITERIM = 20;
TSPAN1    = linspace(t0,t1,n+1);
TSPAN2    = linspace(t1,t0,n+1);
[T,X]     = ode23(F,TSPAN1,X0);
X         = X';
J         = feval(H,1);
ITER = 0; DONE = 0;
while ~DONE
   ITER   = ITER + 1;
   V      = U;
   [T,Y]  = ode23(G,TSPAN2,YT);
   Y      = fliplr(Y');
   H_u    = feval(H,2);
   RD     = feval(H,3);
   Z      = feval('grad02a',RD,epsilon);
   U      = V - epsilon*(H_u + Z);
   [T,X]  = ode23(F,TSPAN1,X0);
   X      = X';
   J1     = feval(H,1);
   eps1   = epsilon;
   ITERIM = 0;
   while J1 > J & ITERIM <= MAXITER
      ITERIM = ITERIM + 1;
      eps1   = eps1/2;
      U      = V - eps1*(H_u + Z);
      [T,X]  = ode23(F,TSPAN1,X0);
      X      = X';
      J1     = feval(H,1);
   end
   epsilon = 5*eps1;
   DIFF   = abs(J1 - J);
   J      = J1;
   ITER_ITERIM_J = [ITER,ITERIM,J]
   DONE   = DIFF < tol | ITER > MAXITER | ITERIM > MAXITER;
end
