function Y = bsp16(X,flag,Parmeter);
% Himmelblau, p. 395
% flag = 1: Objective function
% flag = 2: Inequalities
% flag = 3: Equalities
% flag = 4: Gradient of objective function
% flag = 5: Gradient of inequalities
% flag = 6: Gradient of equalities
% -------------------------------------------
% Gradient of f: R_n -> R_m is (m,n)-matrix
% -------------------------------------------
c   = [-6.089; -17.164; -34.054; -5.914; -24.721;...
     -14.986; -24.100; -10.708; -26.662; -22.179];
SUM = sum(X);
AUX = real(log(X/SUM));
switch flag
case 1, Y   = X'*(c + AUX);
case 2, Y = X;
case 3
   h1 = X(1) + 2*X(2) + 2*X(3) + X(6) + X(10) - 2;
   h2 = X(4) + 2*X(5) + X(6) + X(7) - 1;
   h3 = X(3) + X(7) + X(8) + 2*X(9) + X(10) - 1;
   Y = [h1; h2; h3];
case 4
   Y = (c + AUX)' + (ones(1,length(X)) - X')/SUM;
case 5, Y = eye(length(X));
case 6
   gradh1 = [1, 2, 2, 0, 0, 1, 0, 0, 0, 1];
   gradh2 = [0, 0, 0, 1, 2, 1, 1, 0, 0, 0];
   gradh3 = [0, 0, 1, 0, 0, 0, 1, 1, 2, 1];
   Y = [gradh1; gradh2; gradh3];
end;
