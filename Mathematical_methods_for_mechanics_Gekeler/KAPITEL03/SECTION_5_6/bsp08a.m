function Y = bsp08(X,flag,Parmeter);
% Himmelblau, S. 397
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
   Y = 1000 -  X(1)^2 - 2*X(2)^2 - X(3)^2;
   Y = Y - X(1)*X(2) - X(1)*X(3);
case 2
   Y = [X(1); X(2); X(3)];
case 3
   h1 = X(1)^2 + X(2)^2 + X(3)^2 - 25;
   h2 = 8*X(1) + 14*X(2) + 7*X(3) - 56;
   Y = [h1; h2];
case 4, Y = derivative(@bsp08,X,1,hh,Parmeter);
case 5, Y = derivative(@bsp08,X,2,hh,Parmeter);
case 6, Y = derivative(@bsp08,X,3,hh,Parmeter);
end
