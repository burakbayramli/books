function [m, k, r] = TransientPlaneFrameElement(modulus, inertia, ...
    A, rho, qs, qt, coord)
% Plane frame element for dynamic analysis
% modulus = modulus of elasticity
% inertia = moment of inertia
% A = area of cross-section
% rho = mass density
% qs = distributed load along the element axis
% qt = distributed load normal to the element axis
% coord = coordinates at the element ends

EI=modulus*inertia; EA = modulus*A;
x1=coord(1,1); y1=coord(1,2);
x2=coord(2,1); y2=coord(2,2);
L=sqrt((x2-x1)^2+(y2-y1)^2);
ls=(x2-x1)/L; ms=(y2-y1)/L;

T = [ls, ms, 0, 0, 0, 0;
    -ms, ls, 0, 0, 0, 0;
    0, 0, 1, 0, 0, 0;
    0, 0, 0, ls, ms, 0;
    0, 0, 0, -ms, ls, 0;
    0, 0, 0, 0, 0, 1];
kl = [EA/L, 0, 0, -(EA/L), 0, 0;
    0, (12*EI)/L^3, (6*EI)/L^2, 0, ...
        -((12*EI)/L^3), (6*EI)/L^2;
      0, (6*EI)/L^2, (4*EI)/L, 0, ...
          -((6*EI)/L^2), (2*EI)/L;
      -(EA/L), 0, 0, EA/L, 0, 0;
      0, -((12*EI)/L^3), -((6*EI)/L^2), 0, ...
          (12*EI)/L^3, -((6*EI)/L^2);
      0, (6*EI)/L^2, (2*EI)/L, 0, ...
          -((6*EI)/L^2), (4*EI)/L];
ml = ((rho*A*L)/420)*[140,0, 0, 70, 0, 0;
    0, 156, 22*L, 0, 54, -13*L;
    0, 22*L, 4*L^2, 0, 13*L, -3*L^2;
    70, 0, 0, 140, 0, 0;
    0, 54, 13*L, 0, 156, -22*L;
    0, -13*L, -3*L^2, 0, -22*L, 4*L^2];
rl = [qs*(L/2); qt*(L/2); qt*(L^2/12); 
        qs*(L/2); qt*(L/2); -qt*(L^2/12)];
m=T'*ml*T; k=T'*kl*T; r=T'*rl;