function Y = bsp01(X,flag,Parmeter3);
% Zermelo's Problem, cf. BRYSON-HO, p. 76, Kostate eliminated
% S velocity of ship
% flag = 1: Function F
% flag = 2: Boundary condition
% flag = 3: Gradient of F
% flag = 4: Gradient of boundary condition

hh = 1E-5; % increment for calculation of derivative
AW = Parmeter3(1:2); BW = Parmeter3(3:4);
S  = Parmeter3(5);
[F,FD,FDD] = bsp03c(X,S);
C = cos(X(4));
D = sin(X(4));
switch flag
case 1
   Y    = zeros(4,1);
   Y(1) = (S*cos(X(4)) + F(1))*X(3);
   Y(2) = (S*sin(X(4)) + F(2))*X(3);
   Y(3) = 0;
   Y(4) = (D^2*FD(2,1)+C*D*(FD(1,1)-FD(2,2))-C^2*FD(1,2))*X(3);
case 2
   Y  = [X(1:2) - AW; X(5:6) - BW];
case 3
   Y = derivative(@bsp03,X,1,hh,Parmeter3);
case 4
   Y = derivative(@bsp03,X,2,hh,Parmeter3);
end
