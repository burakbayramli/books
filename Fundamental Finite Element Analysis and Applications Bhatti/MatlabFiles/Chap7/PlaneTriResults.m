function se = PlaneTriResults(type, e, nu, alpha, deltaT, coord, dn)
% se = PlaneTriResults(typ, e, nu, alpha, deltaT, coord, dn)
% Computes element solution for a plane stress/strain triangular element
% e = modulus of elasticity
% nu = Poisson's ratio
% alpha = coefficient of thermal expansion
% deltaT = temperature change
% coord = nodal coordinates
% dn = nodal displacements
% Following are the output variables are at element center
% {strains, stresses, principal stresses, effective stress}
x1=coord(1,1); y1=coord(1,2);
x2=coord(2,1); y2=coord(2,2);
x3=coord(3,1); y3=coord(3,2);
x=(x1+x2+x3)/3; y=(y1+y2+y3)/3;
switch (type)
case 1
    e0 = alpha*deltaT*[1; 1; 0];
    C = e/(1 - nu^2)*[1, nu, 0; nu, 1, 0; 0, 0, (1 - nu)/2];
case 2
    e0 = (1 + nu)*alpha*deltaT*[1; 1; 0];
    C = e/((1 + nu)*(1 - 2*nu))*[1 - nu, nu, 0; nu, 1 - nu, 0;
        0, 0, (1 - 2*nu)/2];
end

b1 = y2 - y3; b2 = y3 - y1; b3 = y1 - y2;
c1 = x3 - x2; c2 = x1 - x3; c3 = x2 - x1;
f1 = x2*y3 - x3*y2; f2 = x3*y1 - x1*y3; f3 = x1*y2 - x2*y1;
A = (f1 + f2 + f3)/2;
B = [b1, 0, c1; 0, c1, b1; b2, 0, c2; 0, c2, b2;
    b3, 0, c3; 0,c3, b3]/(2*A);
eps = B'*dn;
sig = C*(eps-e0)
sx = sig(1); sy= sig(2); sxy=sig(3);
PrincipalStresses = eig([sx,sxy; sxy,sy])
se = sqrt((sx - sy)^2 + sy^2 + sx^2 + 6*sxy^2)/sqrt(2); 