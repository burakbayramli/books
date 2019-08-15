function Y = bsp02(X,flag,Parmeter);
% Test example of Rheinboldt
% flag = 1: Function
% flag = 2: Gradient of Function

switch flag
case 1
   Y = [X(1) - X(2)^3 + 5*X(2)^2 - 2*X(2) + 34*X(3) - 47;
        X(1) + X(2)^3 + X(2)^2 - 14*X(2) + 10*X(3) - 39];
case 2
   Y =  [1, -3*X(2)^2+10*X(2)-2, 34;
         1, 3*X(2)^2+2*X(2)-14,  10];
end
