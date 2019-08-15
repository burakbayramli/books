function demo2
% Bifurcation for algebraic systems
% bsp02 : Example of Crandall;
% with X_0 ~= [0,0] and Newton's method

clc, format compact, format short
Beispiel = 100;
while ~ismember(Beispiel,[1,2,3,4,5,6])
   Beispiel = input(' Example no. (1/2/3/4/5/6)? ');
end;
disp(' Example of Crandall')
   F     = 'bsp02';
   n     = 2;     % Dimension of alg. system
   tol   = 1E-6;  % Toleranz
   maxit = 5;     % Max. step number in iteration
   Eps   = 0.6;   % Scaling parameter, to CHOOSE!
   K     = 1;     % Nr. of tangental vector U_K in UU
switch Beispiel
case 1, disp(' MU_0 = 1')
   FALL = 1; NU = 1; K = 1; X0 = [0;0];
case 2, disp(' MU_0 = 10')
   FALL = 2; NU = 1; K = 1; X0 = [0;0];
case 3, disp(' MU_0 = 4, U0 = [1;0]')
   FALL = 3; NU = 1; K = 1; X0 = [sqrt(3)/2;0];
case 4, disp(' MU_0 = 4, U0 = [0;1] ')
   FALL = 4; NU = 1; K = 1; X0 = [sqrt(3)/2;0]; UU = zeros(n,1);
case 5, disp(' MU_0 = 5.5')
   FALL = 5; NU = 1; K = 1; X0 = [0;3/sqrt(11)]; UU = zeros(n,1);
case 6, disp(' MU_0 = 5.5')
   FALL = 6; NU = 1; K = 1; X0 = [0;3/sqrt(11)]; UU = zeros(n,1);
end
% -- Start values -----------------------
Parmeter = [NU;FALL;n;Eps;zeros(n+1,1)];
MU0 = feval(F,zeros(n+1,1),3,Parmeter,zeros(n,1)); % Eigenvalue
UU  = feval(F,zeros(n+1,1),4,Parmeter,zeros(n,1)); % Eigenvector
U   = UU(:,K); Parmeter(5) = MU0;
Parmeter = [Parmeter;X0];
XSTART    = [X0 + Eps*U;MU0];
[Y,ecode] = newton(F,XSTART,tol,maxit,Parmeter,U);
M  = length(Y); MU = Y(M); Y = Y(1:M-1);
disp(' ---------------- ')
MU_MAXNORMY  = [MU,max(abs(Y))]
save daten2 Y MU FALL
bild02
