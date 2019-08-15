function W = bsp03(T,X,Parmeter);
% Differential system for two-body problem
% dimensionless problem
% in cartesian coordinates
m1 = Parmeter(1); m2 = Parmeter(2); 
Y = X(3:4);
denominator  = (sqrt((X(1) - X(2))^2 + (Y(1) - Y(2))^2))^3;
W = zeros(8,1);
W(1:4) = X(5:8);
W(5) = m2*(X(2) - X(1))/denominator;
W(6) = m1*(X(1) - X(2))/denominator;
W(7) = m2*(Y(2) - Y(1))/denominator;
W(8) = m1*(Y(1) - Y(2))/denominator;
