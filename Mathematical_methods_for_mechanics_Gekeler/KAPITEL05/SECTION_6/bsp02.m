function Y = bsp02(X,flag,Parmeter);
% Beispiel von Crandall
% flag = 1: Funktion
% flag = 2: Gradient
% flag = 3: Eigenwert
% flag = 4: Eigenvektor
% -------------------------------------------
% Gradient von f: R_n -> R_m ist (m,n)-Matrix
% A ist die Diskretisierung des negative Laplace-Operators
% mit Finiten Differenzen
% --Parameter ---------------------------------
n = length(X) - 1;
CASE = Parmeter(1); Eps  = Parmeter(2);
NU   = Parmeter(3); K    = Parmeter(4);
MU0  = Parmeter(5);
switch CASE
case 1, UU = [1; 0];
case 2, UU = [0; 1];
case 3, UU = [0; 1];
end
X1   = X(1:n); MU = X(n+1);
X0   = [sqrt(3)/2; 0];
X2 = Eps*UU(:,K) + X1;      % wenn X0 = (0,0)
if CASE == 3
   X0   = [sqrt(3)/2; 0];
   X2 = X0 + Eps*UU(:,K) + X1; % wenn X0 ~= (0,0)
end
% ----------------------------------
A = [1, 0; 0, 10];
switch flag
case 1
   Y = [A*X2 + (MU0 + MU)*func(X2);zeros(NU,1)];
case 2
   GRADF = grad(X2);
   Y = [A + (MU0 + MU)*GRADF, func(X2)];
   Y = [Y;UU',zeros(NU,1)];
case 3
   switch CASE
   case 1, Y = 1;
   case 2, Y = 10;
   case 3, Y = 4;
   end
case 4, Y = UU;
end

function Y = func(X)
Y = [X(1)^3 - X(1) + X(1)*X(2)^2; -(X(2) + 2*X(1)^2*X(2) + X(2)^3)];

function Y = grad(X)
Y = [3*X(1)^2 - 1 + X(2)^2, 2*X(1)*X(2);
     -4*X(1)*X(2), -(1 + 2*X(1)^2 + 3*X(2)^2)];

