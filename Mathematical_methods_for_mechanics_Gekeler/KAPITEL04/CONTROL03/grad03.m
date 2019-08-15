function errorcode = grad03(name,X0,YT,tol,Y,Y1,Y2)
% Eckart Gekeler, Universitaet Stuttgart, Release 17.4.05
% Gradientenverfahren fuer Kontrollprobleme
% zwei zusaetzliche Endbedingungen

global d n kappa t1 U X
F = [name '_f']; G = [name '_g']; H = [name '_h'];
%options   = odeset('RelTol',1e-2,'AbsTol',[1e-2 1e-2 1e-2], ...
%            'OutputFcn','odeplot');
options   = odeset('RelTol',1e-6,'AbsTol',1e-6);
errorcode = 0; epsilon = 1;
MAXITER = 150; MAXITERIM = 50;
TSPAN1 = linspace(0,t1,n+1); TSPAN2 = linspace(t1,0,n+1);
[T,X]     = ode45(F,TSPAN1,X0,options);
X         = X';
J         = feval(H,1,Y,Y1,Y2);
ITER = 0; DONE = 0;
while ~DONE
   ITER   = ITER + 1;
   V      = U;
   [T,Y]  = ode45(G,TSPAN2,YT,options);
   RB     = feval(H,6,Y,Y1,Y2);
   [T,Y1] = ode45(G,TSPAN2,RB(:,1),options);
   [T,Y2] = ode45(G,TSPAN2,RB(:,2),options);
   Y      = fliplr(Y');
   Y1     = fliplr(Y1');
   Y2     = fliplr(Y2');
   H0_u   = feval(H,2,Y,Y1,Y2);
   H1_u   = feval(H,3,Y,Y1,Y2);
   Z      = grad03a(H,H0_u,H1_u,epsilon);
   DU     = H0_u + Z*H1_u;
   U      = V - epsilon*DU;
   [T,X]  = ode45(F,TSPAN1,X0,options,U);
   X      = X';
   J1     = feval(H,1,Y,Y1,Y2);
   eps1   = epsilon;
   ITERIM = 0;
   while J1 > J & ITERIM <= MAXITERIM
      ITERIM = ITERIM + 1;
      eps1   = eps1/2;
      [T,Y]  = ode45(G,TSPAN2,YT,options);
      RB     = feval(H,6,Y,Y1,Y2);
      [T,Y1] = ode45(G,TSPAN2,RB(:,1),options);
      [T,Y2] = ode45(G,TSPAN2,RB(:,2),options);
      Y      = fliplr(Y');
      Y1     = fliplr(Y1');
      Y2     = fliplr(Y2');
      H0_u   = feval(H,2,Y,Y1,Y2);
      H1_u   = feval(H,3,Y,Y1,Y2);
      Z      = grad03a(H,H0_u,H1_u,2);
%     Z      = grad03a(H,H0_u,H1_u,eps1);
      DU     = H0_u + Z*H1_u;
      U      = V - eps1*DU;
      [T,X]  = ode45(F,TSPAN1,X0,options);
      X      = X';
      J1     = feval(H,1,Y,Y1,Y2);
   end
   epsilon   = min(1,2*eps1);
   DIFF      = abs(J1 - J);
   RB        = feval(H,7,Y,Y1,Y2);
   J_R_IT    = [J1,RB(1),RB(2),ITERIM,ITER]
   J         = J1;
   DONE      = DIFF < tol | ITER > MAXITER | ITERIM  > MAXITERIM;
   if ITER > MAXITER,  errorcode = 1; end
   if ITERIM >  MAXITERIM, errorcode = 2; end
end
save daten t1 T X U
