% GAUSS1 The first geodetical main problem solved iteratively 
%        by means of Gauss' mid-latitude formulas.
%
% Given the coordinates (phi1, lambda1) of a point and a
% distance s and an azimuth az1 from here.
% The coordinates (phi2,lambda2) and the reverse azimuth az2 are unknowns.

%Kai Borre 02-19-94
%Copyright (c) by Kai Borre
%$Revision: 1.0 $ $Date: 1997/10/15  

disp('Reference Ellipsoid for Geographical Coordinates');
disp('1. International Ellipsoid 1924');
disp('2. International Ellipsoid 1967');
disp('3. World Geodetic System 1972');
disp('4. Geodetic Reference System 1980');
disp('5. World Geodetic System 1984');
i = input('Select Number of Reference Ellipsoid (1-5): ');
if ((i <= 0) | (i > 5)), break, end
phi1 = input('phi1 = (in the format [56 34 17.4321]): ');
b1 = phi1(1)+phi1(2)/60+phi1(3)/3600;
b1 = b1*pi/180;
lambda1 = input('lambda1 = (in the format [ 9 57 34.4877]): ');
l1 = lambda1(1)+lambda1(2)/60+lambda1(3)/3600;
l1 = l1*pi/180;
az1 = input('az1 = (in the format [302 10 33.4479]): ');
a1 = az1(1)+az1(2)/60+az1(3)/3600;
a1 = a1*pi/180;
s = input('s = (in m): ');
a = [6378388 6378160 6378135 6378137 6378137];
f = [1/297 1/298.247 1/298.26 1/298.257222101 1/298.257223563];
ex2 = (2-f(i))*f(i)/((1-f(i))^2);
V2 = 1+ex2*(cos(b1))^2;
N1 = a(i)*sqrt((1+ex2)/V2);
l2 = l1+s*sin(a1)/(N1*cos(b1));
b2 = b1+V2*s*cos(a1)/N1;
olddb = 0.1; db = 0;
while abs(olddb-db) > 1.e-14
   b = (b1+b2)/2;
   db = b2-b1;
   olddb = db;
   b2 = (db)^2;
   dl = l2-l1;
   t2 = (sin(b)/cos(b))^2;
   eta2 = ex2*(cos(b))^2;
   V2 = 1+eta2;
   V4 = V2*V2;
   N1 = a(i)*sqrt((1+ex2)/V2);
   deltaA = dl*sin(b)*(1+(1+eta2)*(dl*cos(b))^2/12 ...
                                   +(3+8*eta2)*b2/(24*V4));
   az = a1+deltaA/2;
   dl = s*sin(az)*(1+(dl*sin(b))^2/24 ...
                -(1+eta2-9*eta2*t2)*b2/(24*V4))/(N1*cos(b));
   db = V2*s*cos(az)*(1-(1-2*eta2)*(dl*cos(b))^2/24 ...
                      -eta2*(1-t2)*b2/(8*V4))/(N1*cos(dl/2));
   b2 = b1+db;
   l2 = l1+dl;
end

phi2 = (b1+db)*180/pi;
lambda2 = (l1+dl)*180/pi;
azimuth2 = (az+deltaA/2)*180/pi+180;

bo = zeros(1,3);
bo(1) = fix(phi2);
bo(2) = fix(rem(phi2,bo(1))*60);
bo(3) = (phi2-bo(1)-bo(2)/60)*3600;
lo = zeros(1,3);
lo(1) = fix(lambda2);
lo(2) = fix(rem(lambda2,lo(1))*60);
lo(3) = (lambda2-lo(1)-lo(2)/60)*3600;
az = zeros(1,3);
az(1) = fix(azimuth2);
az(2) = fix(rem(azimuth2,az(1))*60);
az(3) = (azimuth2-az(1)-az(2)/60)*3600;
if az(1) > 360, az(1) = az(1)-360; end
   
fprintf('\n      phi2 = %3.0f %2.0f %8.5f',bo(1),bo(2),bo(3))
fprintf('\n   lambda2 = %3.0f %2.0f %8.5f',lo(1),lo(2),lo(3))
fprintf('\n  azimuth2 = %3.0f %2.0f %8.5f\n',az(1),az(2),az(3))
%%%%%%%%%%%%%%% end gauss1.m  %%%%%%%%%%%%%%%%%%%%%%%