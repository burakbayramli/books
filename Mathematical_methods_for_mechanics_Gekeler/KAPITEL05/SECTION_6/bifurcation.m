function [Y,U0,MU,errorcode] = bifurcation(F,G,Parmeter);
% Bifurcation starting from an eigenvector as in DEMO2
global A UU VV W

MU0 = Parmeter(1); NU = Parmeter(2); SIGN = Parmeter(3);
n   = Parmeter(4); K = Parmeter(5);
tol = Parmeter(6); Maxit = Parmeter(7);
Eps = Parmeter(8); m = Parmeter(9);  cc = Parmeter(10);
type = Parmeter(11);
N   = n*m;
W   = zeros(N,1); LAMBDA   = 0;
ZETA = zeros(NU,1); ZETA(K) = 1;
errorcode = 0; iter = 0; done = 0;
% ---------------------------
while ~done
   iter = iter + 1;
   X0 = [LAMBDA;ZETA];
   [Z,errorcode] = newton(G,X0,tol,Maxit,Parmeter);
   LAMBDA = Z(1); ZETA = Z(2:end); U0 = Eps*UU*ZETA;
   Y = U0 + W;
   switch type
   case 1,
      Parmeter(1) = MU0 + LAMBDA; % = MU
      RS2 = feval(F,Y,3,Parmeter);
      RS2 = LAMBDA*Y + RS2;
   case 2, 
      Parmeter(1) = MU0/(1 - LAMBDA); % = MU
      RS2 = feval(F,Y,3,Parmeter);
      RS2 = (Parmeter(1) - MU0)*Y + RS2;
   end
   RS2   = [RS2;zeros(NU,1)];
   L     = [A-MU0*eye(N);VV];
   W     = L\RS2;
   Wnorm = max(abs(W)); 
   %   RES = L*W - RS2; %RES = RES(1:end-1);
   %   Resnorm = max(abs(RES))
   done  = iter >= Maxit;
   ITER_WNORM_LAMBDA = [iter,Wnorm,LAMBDA]
   %pause
end
if iter >= Maxit, errorcode = 1; end
WEIGHTS_OF_EIGENVECTORS = ZETA.'
Y = U0 + W; MU = Parmeter(1);
%RES = feval(F,Y,1,Parmeter);  
%Max_Norm_Residuum = max(abs(RES))

%RESEP = (A - MU0*eye(N))*U0; % O.K.
%NormResEP = norm(RESEP);
