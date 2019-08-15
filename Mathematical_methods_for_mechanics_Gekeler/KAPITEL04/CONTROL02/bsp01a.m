function Y = bsp01a(X,flag,Parmeter3);
% Thrust problem, cf. Bryson-Ho, p. 59.
% numerically calculated derivatives
% flag = 1: Function F
% flag = 2: Boundary condition
% flag = 3: Gradient of F
% flag = 4: Gradient of boundary condition

hh = 1E-5; % increment for calculation of derivative
Height = Parmeter3;
switch flag
case 1
   Y    = zeros(8,1);
   Y(1) = X(3); Y(2) = X(4);
   Y(3) = 1/sqrt(1 + X(8)*X(8));
   Y(4) = X(8)/sqrt(1 + X(8)*X(8));
   Y(5) = 0; Y(6) = 0;
   Y(7) = - X(5); Y(8) = - X(6);
case 2   
   D1  = [1 0 0 0 0 0 0 0; 0 1 0 0 0 0 0 0;
          0 0 1 0 0 0 0 0; 0 0 0 1 0 0 0 0];
   D2  = [0 1 0 0 0 0 0 0; 0 0 0 1 0 0 0 0;
          0 0 0 0 1 0 0 0; 0 0 0 0 0 0 1 0];
   B = [Height; 0; 0; 1];
   Y  = [D1*X(1:8); D2*X(9:16) - B];
case 3   
   Y = derivative(@bsp01,X,1,hh,Parmeter3);
case 4
   Y = derivative(@bsp01,X,2,hh,Parmeter3);
end
