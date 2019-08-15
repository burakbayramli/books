function Y = bsp03(X,flag,Parmeter);
% Chemical reaction model after Kubizek, vgl. Bank/Mittelmann
% flag = 1: Function
% flag = 2: Gradient of function

switch flag
case 1
   Y = [X(5)*(1 - X(3))*ee(X(1)) - X(3);
       22*X(5)*(1 - X(3))*ee(X(1)) - 30*X(1);
       X(3) - X(4) + X(5)*(1 - X(4))*ee(X(2));
       10*X(1) - 30*X(2) + 22*X(5)*(1-X(4))*ee(X(2))];
case 2
   Y1 =  [dee(X(1))*X(5)*(1-X(3)), 0, ...
          -1-X(5)*ee(X(1)), 0, (1-X(3))*ee(X(1))];
   Y2 =  [-30+dee(X(1))*22*X(5)*(1-X(3)), 0,-ee(X(1))*22*X(5), ...
          0, 22*(1 - X(3))*ee(X(1))];
   Y3 = [ 0, dee(X(2))*X(5)*(1 - X(4)), 1, -1 - ee(X(2))*X(5), ...
          (1 - X(4))*ee(X(2))];
   Y4 = [10, -30+dee(X(2))*22*X(5)*(1-X(4)), 0, -22*X(5)*ee(X(2)), ...
          22*(1 - X(4))*ee(X(2))];
   Y = [Y1;Y2;Y3;Y4];
end

function E = ee(X)
E = exp(10*X/(1 + 0.01*X));

function DE = dee(X)
DE = 10*exp(10*X/(1 + 0.01*X))/(1 + 0.01*X)^2;
