 function Y = bsp01(T,X,Parmeter);
% Winkel = X(1); Radiusvektor = X(2); alfa = 1 fuer Ellipse
gamma = Parmeter(1); M = Parmeter(2); m = Parmeter(3);
alfa = Parmeter(4);
Y1 = X(3); Y2 = X(4);
Y3 = - 2*X(3)*X(4)/X(2);
Y4 = X(2)*X(3)*X(3) - gamma*m*M*alfa/X(2)^(alfa+1);
Y = [Y1;Y2;Y3;Y4];
