function Y = bsp06(X,flag,Parmeter);
% flag = 1: Objective function
% flag = 2: Inequality constraints

% flag = 3: Equality constraints
% flag = 4: Boundary of feasible region
% --------------------------------------
switch flag
case 1
   Y = (X(1) - 8)^2/4 + (X(2) - 1)^2;
case 2
   Y = zeros(3,1);
   Y(1) = 25 - (X(1) - 4)^2 - X(2)^2;
   Y(2) = 30 - (X(1) + 1)^2 - (X(2) + 3)^2;
   Y(3) = 30 - (X(1) + 1)^2 - (X(2) - 3)^2;
case 3, Y = [];   
case 4
   X1 = linspace(-0.35,3.6,20);
   Y1 = 3 - sqrt(30 - (X1 + 1).^2);
   X2 = fliplr(X1);
   Y2 = -3 + sqrt(30 - (X2 + 1).^2);
   Y3 = linspace(2.45,-2.45,20);
   X3 = 4 - sqrt(25 - Y3.^2);
%   Y = [[X1,X2,X3];[Y1,Y2,Y3]];
Y = [X1;Y1;X2;Y2;X3;Y3];
end
