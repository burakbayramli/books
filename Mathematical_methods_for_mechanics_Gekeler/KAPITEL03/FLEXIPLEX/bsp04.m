function Y = bsp04(X,flag,Parmeter);
% Spellucci, S. 397
% flag = 1: objective function
% flag = 2: inequality constraints
% flag = 3: equality constraints 
% flag = 4: Boundary of feasible region
% -------------------------------------------
a = X(1) + X(2) - 3.5;
b = X(2) - X(1) + 0.5;
switch flag
case 1
   Y = a^2 + 4*b^2; Y = 100/Y;
case 2
   Y = zeros(1,2);
   Y(1) = 1 + X(2) - X(1)^2;
   Y(2) = 1 - X(2) - X(1)^2;
case 3, Y = [];   
case 4   
   X = linspace(-1,1,20);
   Y1 = X.*X - 1;
   Y2 = 1 - X.*X;
   Y = [X;Y1;Y2];
end
