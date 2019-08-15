function demo1
% Bifurcation for algebraic systems
% bsp01 : A*x - mu*x + SIGN*x^3, A = 1;

clc, format compact
disp(' Simple example: Pitchfork bifurcation ')
F     = 'bsp01';
NU    = 1;     % Dim Ker L
SIGN  = 1;     % 1
n     = 1;     % Dimension of alg. system
tol   = 1E-6;  % Toleranz
maxit = 5;     % Max. step number of iteration
Eps   = 0.8;   % Scaling parameter checked up to Eps = 100;
K     = 1;     % Nr. of tangential vector U_K in UU
               % 1 <= K < = NU to choose!
% -- Start values -----------------------
Parmeter = [NU,SIGN,n,K,Eps,0]; X = zeros(n+1,1); UU = zeros(n,1);
MU0 = feval(F,X,3,Parmeter,UU); % eigenvalue
UU  = feval(F,X,4,Parmeter,UU);  % eigenvector, VV = UU'
save daten1a UU MU0
Parmeter(6) = MU0;
X0        = zeros(length(UU(:,K))+1,1);
[Y,ecode] = newton(F,X0,tol,maxit,Parmeter,UU);
M         = length(Y);
MU        = Y(M);
MAXNORMY  = max(abs(Y(1:M-1)))
Y         = Eps*UU(:,K) + Y(1:M-1);
Y         = Y(1:M-1);
MU        = MU + MU0;
save daten1b Y MU UU Parmeter
bild01
ecode
