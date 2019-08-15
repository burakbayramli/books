function [X1,Y1,errorcode] = grad04(name,tol,X,Y)
% Eckart Gekeler, Universitaet Stuttgart, Release 17.4.05
% Gradientenverfahren fuer Kontrollprobleme

global d n kappa t1 U
F         = [name '_f'];
G         = [name '_g'];
H         = [name '_h'];
%options   = odeset('RelTol',1e-2,'AbsTol',[1e-2 1e-2 1e-2], ...
%            'OutputFcn','odeplot');
options   = odeset('RelTol',1e-6,'AbsTol',1e-6);
errorcode = 0;
epsilon   = 1;
MAXITER   = 150;
MAXITERIM = 50;
TSPAN1    = linspace(0,t1,n+1);
TSPAN2    = linspace(t1,0,n+1);
RBX       = feval(H,2,X,Y);
[T,X]     = ode45(F,TSPAN1,RBX,options);
X         = X';
J         = feval(H,1,X,Y);
ITER      = 0;
DONE      = 0;
while ~DONE
   ITER   = ITER + 1;
   V      = U;
   RBY    = feval(H,3,X,Y);
   [T,Y]  = ode45(G,TSPAN2,RBY,options);
   Y      = fliplr(Y');
   HAM_U  = feval(H,4,X,Y);
   U      = V - epsilon*HAM_U;
   [T,X]  = ode45(F,TSPAN1,RBX,options);
   X      = X';
   J1     = feval(H,1,X,Y);
   eps1   = epsilon;
   ITERIM = 0;
   while J1 > J & ITERIM <= MAXITERIM
      ITERIM = ITERIM + 1;
      eps1   = eps1/2;
      RBY    = feval(H,3,X,Y);
      [T,Y]  = ode45(G,TSPAN2,RBY,options);
      Y      = fliplr(Y');
      HAM_U  = feval(H,4,X,Y);
      U      = V - eps1*HAM_U;
      [T,X]  = ode45(F,TSPAN1,RBX,options);
      X      = X';
      J1     = feval(H,1,X,Y);
   end
   epsilon   = min(1,10*eps1);
   DIFF      = abs(J1 - J);
   RB        = feval(H,5,X,Y);
   J_R_IT    = [J1,RB(1),RB(2),ITERIM,ITER]
   J         = J1;
   DONE      = DIFF < tol | ITER > MAXITER | ITERIM  > MAXITERIM;
   if ITER > MAXITER, errorcode = 1;  end
   if ITERIM >  MAXITERIM, errorcode = 2; end
end
X1 = X; Y1 = Y;
