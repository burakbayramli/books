function Y = bsp02(X,flag,Parmeter);
% Spellucci, p. 457
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
switch flag
case 1
   Y = (X(1) - X(2))^2 + (X(2) - 1)^2;
case 2
   Y = zeros(2,1);
   Y(1) = 1 - X(1)^2 + X(2);
   Y(2) = 1 - X(1)^2 - X(2);
case 3, Y = 0;
case 4
   Y = [2*(X(1)-X(2)), -2*(X(1)-X(2))+2*(X(2)-1)];
case 5
   g1 = [-2*X(1), 1];
   g2 = [-2*X(1), -1];
   Y  = [g1; g2];
case 6, Y = 0;
case 7 % nur fuer Bild
   X = linspace(-1,1,20);
   Y1 = X.*X - 1;
   Y2 = 1 - X.*X;
   Y = [X;Y1;Y2];
end
