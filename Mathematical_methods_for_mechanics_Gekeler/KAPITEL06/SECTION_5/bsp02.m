function Y = bsp02(T,X,Parmeter);
% Differential system for three-body problem
% dimensionsless system
m1 = Parmeter(1); m2 = Parmeter(2); m3 = Parmeter(3);
Z = zeros(12,1);
Y = X(4:6);
DX = X(7:9);
DY = X(10:12);
R12 = sqrt((X(2)-X(1))*(X(2)-X(1)) + (Y(2)-Y(1))*(Y(2)-Y(1)));
R13 = sqrt((X(3)-X(1))*(X(3)-X(1)) + (Y(3)-Y(1))*(Y(3)-Y(1)));
R23 = sqrt((X(3)-X(2))*(X(3)-X(2)) + (Y(3)-Y(2))*(Y(3)-Y(2)));
R12 = R12^3; R13 = R13^3; R23 = R23^3;
Z(1:3) = DX;
Z(4:6) = DY;
Z(7)  = m2*(X(2)-X(1))/R12 + m3*(X(3)-X(1))/R13;
Z(8)  = m3*(X(3)-X(2))/R23 + m1*(X(1)-X(2))/R12;
Z(9)  = m1*(X(1)-X(3))/R13 + m2*(X(2)-X(3))/R23;
Z(10) = m2*(Y(2)-Y(1))/R12 + m3*(Y(3)-Y(1))/R13;
Z(11) = m3*(Y(3)-Y(2))/R23 + m1*(Y(1)-Y(2))/R12;
Z(12) = m1*(Y(1)-Y(3))/R13 + m2*(Y(2)-Y(3))/R23;
Y = Z;

