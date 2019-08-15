function Y = bsp04(t,X,flag,Parmeter);
% Duffing equation
% flag = 1: Function F
% flag = 2: Right side for coupl4ed system
% flag = 3: Boundary condition
% flag = 4: Gradient of boundary condition
T = Parmeter(1); a = Parmeter(2); b = Parmeter(3);
c = Parmeter(4); d = Parmeter(5);
U = [X(2);
     a*X(2) + b*X(1) + c*X(1)^3 + d*cos(2*pi*t)];

switch flag
case 1, Y = T*U;
case 2, % Gekoppeltes System
   Y      = zeros(8,1);
   GRAD   = [ 0,            1;
             b +c*3*X(1)^2, a];
             
   Y(1:2) = T*U;
   Y(3:4) = T*GRAD*X(3:4);
   Y(5:6) = T*GRAD*X(5:6);
   Y(7:8) = T*GRAD*X(7:8);
   Y(7:8) = U + Y(7:8);
case 3
   Y = [X(1)-X(3); X(2)- X(4)];
case 4 % Gradient der Randbedingung
   A =  [1, 0; 0, 1];
   B = -[1, 0; 0, 1];
   Y  =  [A, B];
   Y  =  sparse(Y);
end
