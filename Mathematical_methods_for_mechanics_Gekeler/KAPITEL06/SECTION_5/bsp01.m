function Y = bsp01(T,X,Parmeter);
% Differential system for motion in central field
% in cartesian coordinates
G = Parmeter(1); M = Parmeter(2);
m = Parmeter(3); alfa  = Parmeter(4);
Denominator = sqrt(X(1)*X(1) + X(2)*X(2))^(alfa+2);
Y1 = X(3); Y2 = X(4);
Y3 = - alfa*G*m*M*X(1)/Denominator;
Y4 = - alfa*G*m*M*X(2)/Denominator;
Y  = [Y1;Y2;Y3;Y4];
