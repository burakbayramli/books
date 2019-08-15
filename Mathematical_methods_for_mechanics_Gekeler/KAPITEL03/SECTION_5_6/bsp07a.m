function Y = bsp07(X,flag,Parmeter);
% Himmelblaus, p. 393
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
case 1, Y = (X(1) - 2)^2 + (X(2) - 1)^2;
case 2, Y = - X(1)^2/4 - X(2)^2 + 1;
case 3, Y = X(1) - 2*X(2) + 1;
case 4, Y = derivative(@bsp07,X,1,hh,Parmeter);
case 5, Y = derivative(@bsp07,X,2,hh,Parmeter);
case 6, Y = derivative(@bsp07,X,3,hh,Parmeter);

end




