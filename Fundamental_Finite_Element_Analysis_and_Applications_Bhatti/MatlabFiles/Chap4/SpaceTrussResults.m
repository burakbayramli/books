function results = SpaceTrussResults(e, A, coord, disps)
% results = SpaceTrussResults(e, A, coord, disps)
% Compute space truss element results
% e = modulus of elasticity
% A = Area of cross-section
% coord = coordinates at the element ends
% disps = displacements at element ends

x1=coord(1,1); y1=coord(1,2); z1=coord(1,3);
x2=coord(2,1); y2=coord(2,2); z2=coord(2,3);
L=sqrt((x2-x1)^2+(y2-y1)^2+(z2-z1)^2);
ls=(x2-x1)/L; ms=(y2-y1)/L; ns=(z2-z1)/L;
T=[ls,ms,ns,0,0,0; 0,0,0,ls,ms,ns];
d = T*disps;
eps= (d(2)-d(1))/L;
sigma = e.*eps;
force = sigma.*A;
results=[eps, sigma, force];