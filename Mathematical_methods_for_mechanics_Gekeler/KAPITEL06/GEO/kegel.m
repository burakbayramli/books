function [X,Y,Z1,Z2] = kegel(m,n,h,r)
% INPUT: m : Scheibenanzahl, n : Kreisunterteilung
%        h : Hoehe,          r : Kreisradius
%        A : Drehachse bei Drehung, phi : Drehwinkel
Z       = linspace(0,1,m);
R       = r*Z;
[X,Y,Z] = cylinder(R,n);
Z1      = h*Z;
Z2      = 0*Z + h;
