function Y = bsp01(X,flag,Parmeter,UU);
% Simple Pitchfork bifurcation
% bsp01  : A*x - mu*x + SIGN*x^3, A = 2;

% flag = 1: Function
% flag = 2: Gradient
% flag = 3: Eigenvalue
% flag = 4: Eigenvector
% -------------------------------------------
% Gradient of f: R_n -> R_m ist (m,n)-Matrix
% --Parameter ---------------------------------
NU = Parmeter(1); SIGN = Parmeter(2); n   = Parmeter(3);
K  = Parmeter(4);  Eps = Parmeter(5); MU0 = Parmeter(6);
W  = X(1:n); MU = X(n+1);
X2 = Eps*UU(:,K) + W;
% ----------------------------------
   A = 1;
switch flag
case 1
   Y = [A*X2 - (MU0 + MU)*X2 + SIGN*X2.*X2.*X2;UU'*W];
case 2
   D  = -(MU0 + MU) + SIGN*3*X2.*X2;
   D1 = -X2;
   Y  = [A - D,D1;UU',zeros(NU,1)];
case 3, Y = A;
case 4, Y = 1;
end
