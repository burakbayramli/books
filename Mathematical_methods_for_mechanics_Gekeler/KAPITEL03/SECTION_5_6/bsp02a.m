function Y = bsp02a(X,flag,Parmeter);
% Spellucci, p. 457
% Example bsp02.m with numerically calculated derivatives
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
hh = 1E-5; % increment for calculation of derivative
switch flag
case 1
   Y = (X(1) - X(2))^2 + (X(2) - 1)^2;
case 2
   Y = zeros(2,1);
   Y(1) = 1 - X(1)^2 + X(2);
   Y(2) = 1 - X(1)^2 - X(2);
case 3, Y = 0;
case 4, Y = derivative(@bsp02,X,1,hh,Parmeter);
case 5, Y = derivative(@bsp02,X,2,hh,Parmeter);
case 6, Y = 0;
case 7 % only for figure
   X = linspace(-1,1,20);
   Y1 = X.*X - 1;
   Y2 = 1 - X.*X;
   Y = [X;Y1;Y2];
end
