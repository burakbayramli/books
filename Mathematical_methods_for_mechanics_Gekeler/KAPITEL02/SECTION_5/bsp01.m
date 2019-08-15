function Y = bsp01(T,X,flag,Parmeter3);
% Example Stoer-Bulirsch, Par. 7.3, Bsp01
% flag = 1: Function F
% flag = 2: Gradient of F
% flag = 3: Boundary condition
% flag = 4: Gradient of boundary condition
% flag = 5: Coupled system for multiple shooting method
V = [X(2); 5*sinh(5*X(1))];
switch flag
case 1, Y = V;
case 2,
   Grad = [ 0,              1;
           25*cosh(5*X(1)), 0];
   Y    = Grad;
case 3 % boundary condition
   X_ANF = X(1:2);
   X_END = X(3:4);
   Y  = [X_ANF(1); X_END(1) - 1];
case 4 % Gradient of boundary condition
   D1 = [1, 0; 0, 0];
   D2 = [0, 0; 1, 0];
   Y  = [D1, D2];
case 5 % Coupled system for multiple shooting method
   Y = zeros(6,1);
   Grad = [ 0,              1;
           25*cosh(5*X(1)), 0];
   Y(1:2) = V;
   Y(3:4) = Grad*X(3:4);
   Y(5:6) = Grad*X(5:6);
   Y      = Y;
end
