function Y = bsp01(t,X,flag,Parmeter);
% Nerve membran model
% flag = 1: Function F
% flag = 2: Right side for coupled system
% flag = 3: Boundary condition
% flag = 4: Gradient of boundary condition
T = Parmeter(1);
U = [3*(X(2) + X(1) - X(1)^3/3 - 1);
    - (X(1) - 0.7 + 0.8*X(2))/3];
switch flag
case 1, Y = T*U;
case 2, % Coupled system
   Y      = zeros(8,1);
   GRAD   = [3 - 3*X(1)^2,  3;
             -1/3        , -0.8/3];
   Y(1:2) = T*U;
   Y(3:4) = T*GRAD*X(3:4);
   Y(5:6) = T*GRAD*X(5:6);
   Y(7:8) = T*GRAD*X(7:8);
   Y(7:8) = U + Y(7:8);
case 3
   Y = [X(1)-X(3); X(2)- X(4)];
case 4 % Gradient of boundary condition
   A =  [1, 0; 0, 1];
   B = -[1, 0; 0, 1];
   Y  =  [A, B];
   Y  =  sparse(Y);
end
