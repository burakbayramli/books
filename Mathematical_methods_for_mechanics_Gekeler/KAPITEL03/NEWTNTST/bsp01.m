function Y = bsp01(X,FLAG)
% flag = 1: Function
% flag = 2: Gradient
switch FLAG
case 1
   Y = [exp(X(1))*cos(X(2))-1; 
       exp(X(1))*sin(X(2))];
case 2
   Y =  [exp(X(1))*cos(X(2)), -exp(X(1))*sin(X(2));
        exp(X(1))*sin(X(2)), exp(X(1))*cos(X(2))];
end
