%G2C Convertion of geographical coordinates (phi,lambda,h) on a 
%    selected reference ellipsoid to cartesian coordinates (X,Y,Z)

%Kai Borre 02-19-94
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/10/15  $

disp('Reference Ellipsoid for Geographical Coordinates');
disp('1. International Ellipsoid 1924');
disp('2. International Ellipsoid 1967');
disp('3. World Geodetic System 1972');
disp('4. Geodetic Reference System 1980');
disp('5. World Geodetic System 1984');
i=input('Select Number of Reference Ellipsoid (1-5): ');
if ((i <= 0) | (i > 5)), break, end
phi = input('phi = (in the format [ 56 34 17.4321]): ');
b = phi(1)+phi(2)/60+phi(3)/3600;
b = b*pi/180;
lambda = input('lambda = (in the format [9 57 34.4877]): ');
l = lambda(1)+lambda(2)/60+lambda(3)/3600;
l = l*pi/180;
h = input('h = ');
a = [6378388 6378160 6378135 6378137 6378137];
f = [1/297 1/298.247 1/298.26 1/298.257222101 1/298.257223563];
ex2 = (2-f(i))*f(i)/((1-f(i))^2);
c = a(i)*sqrt(1+ex2);
N = c/sqrt(1+ex2*cos(b)^2);
X = (N+h)*cos(b)*cos(l);
Y = (N+h)*cos(b)*sin(l);
Z = ((1-f(i))^2*N+h)*sin(b);
fprintf('\n X =%12.3f\n Y =%12.3f\n Z =%12.3f\n',X,Y,Z)
%%%%%%%%%%%%%% end g2c.m  %%%%%%%%%%%%%%%%%%%%%%%%
