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
hh = 1E-5; % increment for calculation of derivative
switch flag
case 1
   Y = (4/3)*(X(1)^2 - X(1)*X(2) + X(2)^2)^(3/4) - X(3);
case 2, Y = [X(1); X(2); 2-X(3)];
case 3, Y = 0;
case 4, Y = derivative(@bsp09,X,1,hh,Parmeter);
case 5, Y = derivative(@bsp09,X,2,hh,Parmeter);
case 6, Y = 0;
end
