function test05
% Check of Euler angles numerically
clc
RANDOM = rand(3,1);
phi = RANDOM(1); thet = RANDOM(2); psi = RANDOM(3);
%phi = 0; %thet = 0;
aux = 0;
[D1,D2,D3] = drehmatrix_el(thet,aux,phi);
[E1,E2,E3] = drehmatrix_el(thet,aux,psi);
DA = D3*E1*E3;
% Drehmatrix nach Heil-Kitzka ------------------
DB = ...
    [ cos(psi)*cos(phi) - sin(psi)*cos(thet)*sin(phi), ...
    - sin(psi)*cos(phi) - cos(psi)*cos(thet)*sin(phi), ...
      sin(thet)*sin(phi);
      cos(psi)*sin(phi) + sin(psi)*cos(thet)*cos(phi), ...
    - sin(psi)*sin(phi) + cos(psi)*cos(thet)*cos(phi), ...
    - sin(thet)*cos(phi);
      sin(psi)*sin(thet), cos(psi)*sin(thet), cos(thet)];
DIFF = DA - DB
