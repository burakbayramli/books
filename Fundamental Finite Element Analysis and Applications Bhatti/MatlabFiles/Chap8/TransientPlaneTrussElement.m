function [m, k] = TransientPlaneTrussElement(e, A, rho, coord)
% [m, k] = TransientPlaneTrussElement(e, A, rho, coord)
% Generates mass & stiffness matrices for a plane truss element
% rho = mass density
% e = modulus of elasticity
% A = area of cross-section
% coord = coordinates at the element ends

x1=coord(1,1); y1=coord(1,2);
x2=coord(2,1); y2=coord(2,2);
L=sqrt((x2-x1)^2+(y2-y1)^2);
ls=(x2-x1)/L; ms=(y2-y1)/L;
k = e*A/L*[ls^2, ls*ms,-ls^2,-ls*ms;
    ls*ms, ms^2,-ls*ms,-ms^2;
    -ls^2,-ls*ms,ls^2,ls*ms;
    -ls*ms,-ms^2,ls*ms,ms^2];
m = ((rho*A*L)/6)*[2, 0, 1, 0; 0, 2, 0, 1; 
    1, 0, 2, 0;0, 1, 0, 2];