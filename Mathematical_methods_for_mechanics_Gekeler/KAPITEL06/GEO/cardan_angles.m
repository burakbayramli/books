function cardan_angles
% rotational axis by Cardan angles
clc
syms alf bet gam
D1 = [1, 0, 0; 0, cos(alf), - sin(alf); 0, sin(alf), cos(alf)];
D2 = [cos(bet), 0, -sin(bet); 0, 1, 0; sin(bet)  0, cos(bet)];
D3 = [cos(gam), -sin(gam), 0; sin(gam), cos(gam), 0; 0, 0, 1];
DA = D1*D2*D3;

DB = [cos(bet)*cos(gam), - cos(bet)*sin(gam), -sin(bet);
      cos(alf)*sin(gam)-sin(alf)*sin(bet)*cos(gam), ...
      cos(alf)*cos(gam)+sin(alf)*sin(bet)*sin(gam), - sin(alf)*cos(bet);
      sin(alf)*sin(gam)+cos(alf)*sin(bet)*cos(gam), ...
      sin(alf)*cos(gam)-cos(alf)*sin(bet)*sin(gam), cos(alf)*cos(bet)];

DIFF = DA - DB;
DIFF = simplify(DIFF);

% Drehwinkel ---------------------------------
DX = DB(3,2) - DB(2,3);
DX = simplify(DX)
DY = DB(1,3) - DB(3,1);
DY = simplify(DY)
DZ = DB(2,1) - DB(1,2);
DZ = simplify(DZ)

