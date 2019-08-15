function Y = bsp03(X,flag)
% Test function for minimization, cf. Himmelbau, p. 359
% flag = 1: objective function
% flag = 2: inequality constraints
% flag = 3: equality constraints
switch flag
case 1
   Y = 4*X(1) - X(2)^2 - 12;
case 2
   Y = zeros(1,3);
   Y(1) = 10*X(1) - X(1)^2 + 10*X(2) - X(2)^2 - 34;
   Y(2) = X(1);
   Y(3) = X(2);
case 3
   Y = 25 - X(1)^2 - X(2)^2;
end
