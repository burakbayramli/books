function Y = bsp02(X,flag,Parmeter,U);
% Example of Crandall
% flag = 1: Funktion
% flag = 2: Gradient
% flag = 3: Eigenvalue
% flag = 4: Eigenvector
% -------------------------------------------
% Gradient von f: R_n -> R_m ist (m,n)-Matrix
% --Parameter ---------------------------------
NU = Parmeter(1); FALL = Parmeter(2); n = Parmeter(3);
Eps = Parmeter(4); MU0 = Parmeter(5);
X0 = Parmeter(6:5+n);
X2  = X(1:n); MU = X(n+1);
% ----------------------------------
   A = [1, 0; 0, 10];
switch flag
case 1
   Y = [A*X2 + MU*func(X2);U.'*(X2 - Eps*U - X0)];
case 2
   GRADF = grad(X2);
   Y = [A + MU*GRADF, func(X2)];
   Y = [Y;U.',zeros(NU,1)];
case 3
   switch FALL
   case 1, Y = 1; case 2, Y = 10;
   case 3, Y = 4; case 4, Y = 4; case 5, Y = 5.5; case 6, Y = 5.5;
   end
case 4
   switch FALL
   case 1, Y = [1; 0];
   case 2, Y = [0; 1];
   case 3, Y = [1; 0];
   case 4, Y = [0; 1];
   case 5, Y = [1; 0];
   case 6, Y = [0; 1];
   end
end

function Y = func(X)
Y = [X(1)^3 - X(1) + X(1)*X(2)^2; -(X(2) + 2*X(1)^2*X(2) + X(2)^3)];

function Y = grad(X)
Y = [3*X(1)^2 - 1 + X(2)^2, 2*X(1)*X(2);
     -4*X(1)*X(2), -(1 + 2*X(1)^2 + 3*X(2)^2)];
