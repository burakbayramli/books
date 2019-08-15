function Y = bsp09(X,flag,Parmeter);
% Spellucci, p. 339
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
   Y = (4/3)*(X(1)^2 - X(1)*X(2) + X(2)^2)^(3/4) - X(3);
case 2, Y = [X(1); X(2); 2-X(3)];
case 3, Y = 0;
case 4
   a = (X(1)^2 - X(1)*X(2) + X(2)^2);
   if a == 0
      Y = [0 0 -1];
   else
      a = 1/(a^(1/4));
      Y = [a*(2*X(1)-X(2)), a*(2*X(2)-X(1)), -1];
   end;
case 5, Y = [1 0 0;0 1 0;0 0  -1];
case 6, Y = 0;
end
