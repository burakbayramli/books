function Y = bsp01(X,flag,Parmeter3);
% Thrust problem, cf. Bryson-Ho, p. 59.
% flag = 1: Function F
% flag = 2: Boundary condition
% flag = 3: Gradient of F
% flag = 4: Gradient of boundary condition

Height = Parmeter3;
switch flag
case 1
   Y    = zeros(8,1);
   Y(1) = X(3);
   Y(2) = X(4);
   Y(3) = 1/sqrt(1 + X(8)*X(8));
   Y(4) = X(8)/sqrt(1 + X(8)*X(8));
   Y(5) = 0;
   Y(6) = 0;
   Y(7) = - X(5);
   Y(8) = - X(6);
case 2   
   D1  = [1 0 0 0 0 0 0 0; 0 1 0 0 0 0 0 0;
          0 0 1 0 0 0 0 0; 0 0 0 1 0 0 0 0];
   D2  = [0 1 0 0 0 0 0 0; 0 0 0 1 0 0 0 0;
          0 0 0 0 1 0 0 0; 0 0 0 0 0 0 1 0];
   B = [Height; 0; 0; 1];
   Y  = [D1*X(1:8); D2*X(9:16) - B];
case 3   
   AUX =  (1 + X(8)*X(8))*sqrt(1 + X(8)*X(8));
   Y = sparse(8,8);
   Y(1,3) = 1;
   Y(2,4) = 1;
   Y(3,8) = -X(8)/AUX;
   Y(4,8) = 1/AUX;
   Y(7,5) = - 1;
   Y(8,6) = - 1;
case 4
   D1  = [1 0 0 0 0 0 0 0; 0 1 0 0 0 0 0 0;
          0 0 1 0 0 0 0 0; 0 0 0 1 0 0 0 0];
   D2  = [0 1 0 0 0 0 0 0; 0 0 0 1 0 0 0 0;
          0 0 0 0 1 0 0 0; 0 0 0 0 0 0 1 0];
   D1 = [D1; zeros(4,8)];
   D2 = [zeros(4,8); D2];
   Y = [D1, D2];
end
