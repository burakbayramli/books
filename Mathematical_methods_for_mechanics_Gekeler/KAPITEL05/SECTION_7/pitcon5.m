function [WEG,TANG,SS,errorcode] = pitcon5(F,X0,E0,H0,tol,dir,Parmeter)
% Start for PITCON
% WEG: drei points, TANG : two tangents
errorcode = 0;
N1   = length(X0);
SS   = 0.5;
GRAD = feval(F,X0,2,Parmeter);
A    = [GRAD;E0];
Kond = condest(A);
if Kond > 1.0E8 errorcode = 1; 
   disp(' Choose other vector E0! ');
   return;
end
R  = zeros(N1,1); R(N1) = 1;
W  = A\R;
T1 = W/norm(W);
T1 = dir*T1;
[AUX,I] = sort(abs(T1));
J  = I(N1);
V1 = X0 + H0*T1;
[X1,newtoncode] = newton(F,V1,J,tol,Parmeter);
if newtoncode == 1
   disp(' Choose smaller step length! ')
   errorcode = 1; return
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
GRAD    = feval(F,X1,2,Parmeter);
A       = [GRAD;E0];
Kond    = condest(A);
if Kond > 1.0E8 errorcode = 1; return; end
R       = zeros(N1,1); R(N1) = 1;
W       = A\R;
T2      = W/norm(W);
T2      = dir*T2;
[AUX,I] = sort(abs(T2));
J       = I(N1);
V2      = X1 + H0*T2;
[X2,newtoncode] = newton(F,V2,J,tol,Parmeter);
if newtoncode == 1
   disp(' Choose smaller step length! ')
   errorcode = 1; return
end
WEG = [X0,X1,X2]; TANG = [T1, T2];
