function Y = bsp01(X,flag,Parmeter);
% Spellucci, p. 397
% flag = 1: Objective function
% flag = 2: Inequalities
% flag = 3: Equalities
% flag = 4: Gradient of objective function
% flag = 5: Gradient of inequalities
% flag = 6: Gradient of equalities
% flag = 7: Boundary of feasible domain (2-dim. problems)
% -------------------------------------------
% Gradient of f: R_n -> R_m is (m,n)-matrix
% -------------------------------------------
a = X(1) + X(2) - 3.5;
b = X(2) - X(1) + 0.5;
switch flag
case 1
   Y = a^2 + 4*b^2;
   Y = 100/Y;
case 2
   Y = zeros(2,1);
   Y(1) = 1 + X(2) - X(1)^2;
   Y(2) = 1 - X(2) - X(1)^2;
case 3, Y = 0;
case 4
   u = - 100/(a^2 + 4*b^2)^2;
   Y = u*[2*a - 8*b, 2*a + 8*b];
case 5
   g1 = [-2*X(1), 1];
   g2 = [-2*X(1), -1];
   Y  = [g1; g2];
case 6, Y = 0;
case 7 % only for figure in two-dim. problems
   X = linspace(-1,1,20);
   Y1 = X.*X - 1;
   Y2 = 1 - X.*X;
   Y = [X;Y1;Y2];
end
