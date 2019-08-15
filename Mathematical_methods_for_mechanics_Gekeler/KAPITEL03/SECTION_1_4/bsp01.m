function Y = bsp01(X,flag)
% Test function for minimizatoin, cf. Spellucci, p. 117
% flag = 1: Function F
% flag = 2: Gradient of F (row vector)

R = sqrt(1 + X(1)*X(1) + X(2)*X(2));
switch flag
case 1
   Y = 1.1*X(1)*X(1) + 1.2*X(2)*X(2)...
       -2*X(1)*X(2) - 7*X(1) - 3*X(2) + R;
case 2       
   Y    = zeros(1,2);
   Y(1) = 2.2*X(1) - 2*X(2) + X(1)/R - 7;
   Y(2) = - 2*X(1) + 2.4*X(2)+ X(2)/R - 3;
end
