function Y = bsp15(X,flag,Parmeter);
% Himmelblau, p. 415
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
   Y = -0.5*(X(1)*X(4)-X(2)*X(3)+X(3)*X(9)-X(5)*X(9)+X(5)*X(8)-X(6)*X(7));
case 2
   g1  = 1-X(3)^2-X(4)^2;
   g2  = 1-X(9)^2;
   g3  = 1-X(5)^2-X(6)^2;
   g4  = 1-X(1)^2-(X(2)-X(9))^2;
   g5  = 1-(X(1)-X(5))^2-(X(2)-X(6))^2;
   g6  = 1-(X(1)-X(7))^2-(X(2)-X(8))^2;
   g7  = 1-(X(3)-X(5))^2-(X(4)-X(6))^2;
   g8  = 1-(X(3)-X(7))^2-(X(4)-X(8))^2;
   g9  = 1-X(7)^2-(X(8)-X(9))^2;
   g10 = X(1)*X(4)-X(2)*X(3);
   g11 = X(3)*X(9);
   g12 = -X(5)*X(9);
   g13 = X(5)*X(8)-X(6)*X(7);
   g14 = X(9);
   Y = [g1 g2 g3 g4 g5 g6 g7 g8 g9 g10 g11 g12 g13 g14]';
case 3, Y = 0;
case 4, Y = derivative(@bsp15,X,1,hh,Parmeter);
case 5, Y = derivative(@bsp15,X,2,hh,Parmeter);
case 6, Y = 0;
end
