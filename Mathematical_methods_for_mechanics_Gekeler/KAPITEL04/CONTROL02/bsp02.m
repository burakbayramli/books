function Y = bsp02(X,flag,Parmeter3);
% Orbit problem, cf. Bryson-Ho, p. 66.
% flag = 1: Function F
% flag = 2: Boundary condition
% flag = 3: Gradient of F
% flag = 4: Gradient of boundary condition

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
   AUX    = KAPPA/(X(5)*X(5) + X(6)*X(6))^(3/2);
   X1_2 = X(1)*X(1); X1_3 = X1_2*X(1); X1_4 = X1_3*X(1);

   Y      = sparse(6,6);
   Y(1,2) =   1;
   Y(2,1) =   (2 - X(1)*X(3)*X(3))/X1_3;
   Y(2,3) =   2*X(3)/X(1);
   Y(2,5) =   X(6)*X(6)*AUX*SGN;
   Y(2,6) = - X(5)*X(6)*AUX*SGN;
   Y(3,1) =   X(2)*X(3)/X1_2;
   Y(3,2) = - X(3)/X(1);
   Y(3,3) = - X(2)/X(1);
   Y(3,5) = - X(5)*X(6)*AUX*SGN;
   Y(3,6) =   X(5)*X(5)*AUX*SGN;
   Y(4,1) =   (6*X(5)-2*X(1)*X(3)*X(3)*X(5)...
              + 2*X(1)*X(2)*X(3)*X(6))/X1_4;
   Y(4,2) = - X(3)*X(6)/X1_2;
   Y(4,3) =   (2*X(3)*X(5) - X(2)*X(6))/X1_2;
   Y(4,5) =   (X(1)*X(3)*X(3) - 2)/X1_3;
   Y(4,6) = - X(2)*X(3)/X1_2;
   Y(5,1) = - X(3)*X(6)/X1_2;
   Y(5,3) =   X(6)/X(1);
   Y(5,4) = - 1;
   Y(5,6) =   X(3)/X(1);
   Y(6,1) =   (2*X(3)*X(5) - X(2)*X(6))/X1_2;
   Y(6,2) =   X(6)/X(1);
   Y(6,3) = - 2*X(5)/X(1);
   Y(6,5) = - 2*X(3)/X(1);
   Y(6,6) =   X(2)/X(1);
case 4        
   U  = X(7:12);
   U1_3  = U(1)*U(1)*U(1); U1_5  = U1_3*U(1)*U(1);
   SU1_3 = sqrt(U1_3);
   SU1_5 = sqrt(U1_5);
   A  = [1, 0, 0, 0, 0, 0;
         0, 1, 0, 0, 0, 0;
         0, 0, 1, 0, 0, 0];
   D1 = [A; zeros(3,6)];
   B  = [0,         1, 0, 0, 0, 0;
         0.5/SU1_3, 0, 1, 0, 0, 0;
         0.75*U(6)/SU1_5, 0, 0, 1, 0, -0.5/SU1_3];
   D2 = [zeros(3,6); B];
   Y  = [D1, D2];
   Y  = sparse(Y);
end
