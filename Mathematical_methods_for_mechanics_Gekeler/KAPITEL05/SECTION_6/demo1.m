function demo1
% Bifurcation of algebraic equations
%
clear, clc, format short, format compact
nr = 100;
while ~ismember(nr,[1,2])
   nr = input(' Example no. (1/2)? ');
end;
switch nr
case 1, disp(' Simple Example with three variables ')
   F     = 'bsp01';
   SIGN  = -1;     % or 1 (direction)
   tol   = 1E-6;  % Tolerance
   maxit = 5;     % Max. step number in iteration
   Eps   = 0.8;   % Operational parameter |Eps| < 1 SELECT!
   NU    = 2;     % Dim Ker L: Choose here 1 or 2 
   K     = 1;     % No. of tangent vector U_K in UU
                     % 1 <= K < = NU SELECT!
   % -- Start values -----------------------
   Parmeter = [SIGN,Eps,NU,K,0];
   X0  = zeros(4,1);              % [X;MU]
   MU0 = feval(F,X0,3,Parmeter);  % Eigenvalue
   UU  = feval(F,X0,4,Parmeter);  % Eigenvector(en), VV = UU'
   Parmeter  = [SIGN,Eps,NU,K,MU0];
case 2, disp(' Example of Crandall/Rabinowitz ')
   F     = 'bsp02';
   FALL  = 3;     % Choose 1, 2, or 3 (three different bif. points)
   tol   = 1E-6;  % Tolerance
   maxit = 5;     % Max. step number in iteration
   Eps   = 0.3;   % Operational parameter |Eps| < 1, SELECT!
   NU    = 1;     % Dim Ker L: Choose here 1 or 2
   K     = 1;     % No. of tangent vector U_K in UU
                     % 1 <= K < = NU SELECT!
   % -- Start values -----------------------
   Parmeter = [FALL,Eps,NU,K,0];
   X0  = zeros(3,1);               % [X;MU]
   MU0 = feval(F,X0,3,Parmeter); % Eigenvalue
   UU  = feval(F,X0,4,Parmeter);  % Eigenvector, VV = UU'
   Parmeter  = [FALL,Eps,NU,K,MU0];
end
[Y,ecode] = newton(F,X0,tol,maxit,Parmeter);
M = length(Y); MU = Y(M);
MAXNORMY = max(abs(Y(1:M-1)));
Y  = Eps*UU(:,K) + Y(1:M-1);
MU = MU + MU0;
YY = Y.'
MAXNORMY_MU = [MAXNORMY,MU]
switch nr
case 1, save daten1 Y MU Parmeter
case 2, save daten2 Y MU Parmeter
end
%switch ecode
%case 0, disp(' Solution ')
%case 1, disp(' Bad start vector ')
%case 2, disp(' Bad condition of start vector ')
%case 3, disp(' failure at minmal stepsize ')
%case 4, disp(' maximal number of jacobian eval. exceeded ')
%end
