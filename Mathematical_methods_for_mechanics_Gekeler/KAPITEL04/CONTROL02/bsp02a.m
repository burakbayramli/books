function Y = bsp02(X,flag,Parmeter3);
% Orbit problem, cf. Bryson-Ho, p. 66.
% numerically calculated derivatives
% flag = 1: Function F
% flag = 2: Boundary condition
% flag = 3: Gradient of F
% flag = 4: Gradient of boundary condition

hh = 1E-5; % increment for calculation of derivative
KAPPA = Parmeter3(1);
PP    = Parmeter3(2);
SGN   = sign(X(5)/X(6))*sign(X(5))*sign(X(6));

switch flag
case 1
   AUX  = KAPPA/sqrt(X(5)*X(5) + X(6)*X(6));
   X1_2 = X(1)*X(1); X1_3 = X1_2*X(1);

   Y    = zeros(6,1);
   Y(1) = X(2);
   Y(2) = X(3)*X(3)/X(1) - 1/X1_2 + X(5)*AUX*SGN;
   Y(3) = X(6)*AUX*SGN - X(2)*X(3)/X(1);
   Y(4) = X(5)*X(3)*X(3)/X1_2 - 2*X(5)/X1_3 - X(2)*X(3)*X(6)/X1_2;
   Y(5) = X(3)*X(6)/X(1) - X(4);
   Y(6) = (X(2)*X(6) -2*X(3)*X(5))/X(1);
case 2   
   U = X(7:12);
   SU1_3 = sqrt(U(1)*U(1)*U(1));
   Y = [X(1)-1; X(2); X(3)-1;
        U(2); U(3)-1/sqrt(U(1)); U(4)-0.5*U(6)/SU1_3-PP];
case 3   
   Y = derivative(@bsp02,X,1,hh,Parmeter3);
case 4        
   Y = derivative(@bsp02,X,2,hh,Parmeter3);
end
