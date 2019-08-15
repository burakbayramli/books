function Y = bsp02(t,X,flag,Parmeter);
% Heat flow problem
% flag = 1: Function F
% flag = 2: Right side for coupled system
% flag = 3: Boundary condition
% flag = 4: Gradient of boundary condition
T = Parmeter(1); SIG = Parmeter(2);
B = Parmeter(3); R   = Parmeter(4);
U = [- SIG*(X(1) - X(2));
     X(1)*(R - X(3)) - X(2);
     X(1)*X(2) - B*X(3)];
switch flag
case 1, Y = T*U;
case 2  % Coupled system
   Y        = zeros(5*3,1);
   GRAD     = [  -SIG,  SIG,     0;
               R-X(3),   -1, -X(1);
                 X(2), X(1),   -B];
   Y(1:3)   = T*U;
   Y(4:6)   = T*GRAD*X(4:6);
   Y(7:9)   = T*GRAD*X(7:9);
   Y(10:12) = T*GRAD*X(10:12);
   Y(13:15) = T*GRAD*X(13:15);
   Y(13:15) = U + Y(13:15);
case 3
   Y = [X(1)-X(4); X(2)- X(5); X(3) - X(6)];
case 4 % Gradient of boundary condition
   A  =  [1, 0, 0;
          0, 1, 0;
          0, 0, 1];
   B  = -[1, 0, 0;
          0, 1, 0;
          0, 0, 1];
   Y  = [A, B];
   Y  = sparse(Y);
end
