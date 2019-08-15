function test02
% calculates spherical coordinates
clc
syms phi thet
A = [cos(phi)*cos(thet), -sin(phi), -cos(phi)*sin(thet);
     sin(phi)*cos(thet), cos(phi), - sin(phi)*sin(thet);
     sin(thet), 0, cos(thet)];
%B = det(A);
%C = simplify(B)
D2 = [cos(thet), 0, - sin(thet);
      0,        1,    0;
      sin(thet)  0,   cos(thet)];

D3 = [cos(phi), -sin(phi), 0;
      sin(phi), cos(phi), 0;
      0,         0,       1];
%F = A - D3*D2;
%G = simplify(F)
AA = D2*D3;
AA = simplify(AA)
DETAA = det(AA);
DETAA = simplify(DETAA)

