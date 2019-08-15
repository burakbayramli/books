function Y = bsp01(X,flag,Parmeter3);
% Zermelo's Problem, cf. BRYSON-HO, p. 76, Kostate eliminated
% S velocity of ship
% flag = 1: Function F
% flag = 2: Boundary condition
% flag = 3: Gradient of F
% flag = 4: Gradient of boundary condition

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
   Y    = [FD(1,1)*X(3), FD(1,2)*X(3),  S*C + F(1), -S*D*X(3);
           FD(2,1)*X(3), FD(2,2)*X(3),  S*D + F(2),  S*C*X(3);
           0,       0,           0,           0;
           0,       0,           0,           0];
   SS   = D*D; CS = C*D; CC = C*C;
   Y(4,1) = X(3)*(SS*FDD(2,1)+CS*(FDD(1,1)-FDD(1,2))-CC*FDD(2,1));
   Y(4,2) = X(3)*(SS*FDD(1,2)+CS*(FDD(2,1)-FDD(2,2))-CC*FDD(1,2));
   Y(4,3) = SS*FD(2,1) + CS*(FD(1,1) - FD(2,2)) - CC*FD(1,2);
   Y(4,4) =...
       X(3)*(2*CS*FD(2,1)+(CC+CC)*(FD(1,1)-FD(2,2))+2*CS*FD(1,2));
case 4
   D1 = [eye(2), zeros(2,2); zeros(2,4)];
   D2 = [zeros(2,4); eye(2), zeros(2,2)];
   Y  = [D1, D2];
end
