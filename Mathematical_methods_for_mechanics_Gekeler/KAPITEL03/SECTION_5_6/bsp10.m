function Y = bsp10(X,flag,Parmeter);
% Spellucci, S. 370
% flag = 1: Objective function
% flag = 2: Inequalities
% flag = 3: Equalities
% flag = 4: Gradient of objective function
% flag = 5: Gradient of inequalities
% flag = 6: Gradient of equalities
% -------------------------------------------
% Gradient of f: R_n -> R_m is (m,n)-matrix
% -------------------------------------------
switch flag
case 1
   Y = X(1)^2 + X(2)^2 + 2*X(3)^2 + X(4)^2;
   Y = Y - 5*X(1) - 5*X(2) - 21*X(3) + 7*X(4);
case 2
   Y = zeros(3,1);
   Y(1) = 8 - X(1)^2 - X(2)^2 - X(3)^2 - X(4)^2;
   Y(1) = Y(1) - X(1) + X(2) - X(3) + X(4);
   Y(2) = 10 - X(1)^2 - 2*X(2)^2 - X(3)^2 - 2*X(4)^2;
   Y(2) = Y(2) + X(1) + X(4);
   Y(3) = 5 - 2*X(1)^2 - X(2)^2 - X(3)^2;
   Y(3) = Y(3) -  2*X(1) + X(2) + X(4);
case 3, Y = 0;
case 4, Y = [2*X(1)-5, 2*X(2)-5, 4*X(3)-21, 2*X(4)+7];
case 5
   gradg1 = [-2*X(1)-1 -2*X(2)+1 -2*X(3)-1 -2*X(4)+1];
   gradg2 = [-2*X(1)+1 -4*X(2) -2*X(3) -4*X(4)+1];
   gradg3 = [-4*X(1)-2 -2*X(2)+1 -2*X(3) 1];
   Y = [gradg1; gradg2; gradg3];
case 6, Y = 0;
end
