function results = PlaneTrussResults(e, A, coord, disps)
% results = PlaneTrussResults(e, A, coord, disps)
% Compute plane truss element results
% e = modulus of elasticity
% A = Area of cross-section
% coord = coordinates at the element ends
% disps = displacements at element ends
% The output quantities are eps = axial strain
% sigma = axial stress and force = axial force.

x1=coord(1,1); y1=coord(1,2);
x2=coord(2,1); y2=coord(2,2);
L=sqrt((x2-x1)^2+(y2-y1)^2);
ls=(x2-x1)/L; ms=(y2-y1)/L;
T=[ls,ms,0,0; 0,0,ls,ms];
d = T*disps;
eps= (d(2)-d(1))/L;
sigma = e.*eps;
force = sigma.*A;
results=[eps, sigma, force];