function Y = bsp03(t,X,flag,Parmeter);
% Arenstorf orbit I
% flag = 1: Function F
% flag = 2: Right side for coupled system
% flag = 3: Boundary condition
% flag = 4: Gradient of boundary condition
T  = Parmeter(1); MU = Parmeter(2);
MU1 = 1 - MU;
D1  = (X(1) + MU)^2 + X(3)^2;
D2  = (X(1) - MU1)^2 + X(3)^2;
G1  = D1^(3/2); G2  = D2^(3/2);
H1  = D1^(5/2); H2  = D2^(5/2);
U   = [X(2);
      X(1)+2*X(4) - MU1*(X(1)+MU)/G1 - MU*(X(1)-MU1)/G2;
      X(4);
      X(3)-2*X(2) - MU1*X(3)/G1 - MU*X(3)/G2];
switch flag
case 1, Y = T*U;
case 2
   Y    = zeros(6*4,1);
   Grad = zeros(4,4);
   Grad(2,1) = 1 - MU1*(X(3)^2 - 2*(X(1)+MU)^2)/H1...
                 -  MU*(X(3)^2 - 2*(X(1)-MU1)^2)/H2;
   Grad(4,1) = MU1*3*X(3)*(X(1)+MU)/H1 + MU*3*X(3)*(X(1)-MU1)/H2;
   Grad(1,2) = 1;
   Grad(4,2) = -2;
   Grad(2,3) = MU1*3*X(3)*(X(1)+MU)/H1 + MU*3*X(3)*(X(1)-MU1)/H2;
   Grad(4,3) = 1 - MU1*((X(1)+MU)^2  - 2*X(3)^2)/H1 ...
                  - MU*((X(1)-MU1)^2 - 2*X(3)^2)/H2;
   Grad(2,4) = 2;
   Grad(3,4) = 1;

   Y(1:4)   = T*U;
   Y(5:8)   = T*Grad*X(5:8);
   Y(9:12)  = T*Grad*X(9:12);
   Y(13:16) = T*Grad*X(13:16);
   Y(17:20) = T*Grad*X(17:20);
   Y(21:24) = T*Grad*X(21:24);
   Y(21:24) = U + Y(21:24);
case 3
   Y = [X(1)-X(5); X(2)- X(6); X(3) - X(7); X(4) - X(8)];
case 4 % Gradient of boundary condition
   A  =  [1, 0, 0, 0;
          0, 1, 0, 0;
          0, 0, 1, 0;
          0, 0, 0, 1];
   B  = -[1, 0, 0, 0;
          0, 1, 0, 0;
          0, 0, 1, 0;
          0, 0, 0, 1];
   Y  = [A, B];
   Y  = sparse(Y);
end
