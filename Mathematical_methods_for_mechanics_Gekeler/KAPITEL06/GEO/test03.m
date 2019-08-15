function test03
% calculates rotational axis by Euler angles
clc
syms phi thet pssi % psi fails
D3A = [cos(phi), sin(phi), 0; -sin(phi), cos(phi), 0; 0, 0, 1];
D2 = [cos(thet), 0, - sin(thet); 0, 1, 0; sin(thet)  0, cos(thet)];
D3G = [cos(pssi), sin(pssi), 0; -sin(pssi), cos(pssi), 0; 0, 0, 1];
D = D3A*D2*D3G;              % Eulermatrix
DX = D(3,2) - D(2,3);
DX = simplify(DX)
DY = D(1,3) - D(3,1);
DY = simplify(DY)
DZ = D(2,1) - D(1,2);
DZ = simplify(DZ)
ACHSE = [DX;DY;DZ];
DIFF = D*ACHSE - ACHSE;
DIFF = simplify(DIFF)
