function Y = bsp01(X,flag,Parmeter);
% Einfaches Testbeispiel fuer Bifurkationsverfahren
% flag = 1: Funktion
% flag = 2: Gradient
% flag = 3: Eigenwert
% flag = 4: Eigenvektor
% -------------------------------------------
% Gradient von f: R_n -> R_m ist (m,n)-Matrix
% --Parameter ---------------------------------
n    = length(X)-1;
SIGN = Parmeter(1); Eps  = Parmeter(2);
NU   = Parmeter(3); K    = Parmeter(4);
MU0  = Parmeter(5);
if NU == 1, UU = [0; 0; 1];  end
if NU == 2, UU   = [1, 0; 0, 1; 0, 0];  end
X1   = X(1:n); MU = X(n+1);
X2   = Eps*UU(:,K) + X1;
% ----------------------------------
A = [2, 0, 0; 0, 2 ,0; 0, 0, 1];
switch flag
case 1
   Y = [A*X2 - (MU0 + MU)*(X2 + SIGN*X2.*X2.*X2);
        zeros(NU,1)];
case 2
   D  = 1 + SIGN*3*X2.*X2;
   D1 = X2 + SIGN*X2.*X2.*X2;
   Y  = [A - (MU0 + MU)*diag(D),-D1;
         UU',zeros(NU,1)];
case 3
   if NU == 1, Y = 1; end
   if NU == 2, Y = 2; end
case 4, Y = UU;
end
