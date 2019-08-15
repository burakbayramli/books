function Y = bsp12(X,flag,Parmeter);
% Himmelblau, p. 403
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
   Y = 100*(X(2)-X(1)^2)^2+(1-X(1))^2+90*(X(4)-X(3)^2)^2;
   Y = Y +(1-X(3))^2+10.1*((X(2)-1)^2+(X(4)-1)^2);
   Y = Y +19.8*(X(2)-1)*(X(4)-1);
case 2, e = 10*ones(4,1);
        Y = [X+e;e-X];
case 3, Y = 0;
case 4, Y = derivative(@bsp12,X,1,hh,Parmeter);
case 5, Y = derivative(@bsp12,X,2,hh,Parmeter);
case 6, Y = 0;
end
