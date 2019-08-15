function Y = bsp05(X,flag,Parmeter);
% Spellucci, p. 457
% flag = 1: Objective function
% flag = 2: Inequalities
% flag = 3: Equalities
% flag = 4: Boundary of feasible domain (2-dim. problems)
switch flag
case 1
   Y = (X(1) - X(2))^2 + (X(2) - 1)^2;
case 2
   Y = zeros(2,1);
   Y(1) = 1 - X(1)^2 + X(2);
   Y(2) = 1 - X(1)^2 - X(2);
case 3
   Y = [];
case 4 % only for figure
   X = linspace(-1,1,20);
   Y1 = X.*X - 1;
   Y2 = 1 - X.*X;
   Y = [X;Y1;Y2];
end
