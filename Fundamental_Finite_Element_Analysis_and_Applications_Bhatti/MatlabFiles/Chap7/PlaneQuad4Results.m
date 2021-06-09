function se = PlaneQuad4Results(type, e, nu, alpha, deltaT, coord, dn)
% se = PlaneQuad4Results(type, e, nu, alpha, deltaT, coord, dn)
% Computes element solution for a plane stress/strain quad element
% e = modulus of elasticity
% nu = Poisson's ratio
% alpha = coefficient of thermal expansion
% deltaT = temperature change
% coord = nodal coordinates
% dn = nodal displacements
% Following are the output variables are at element center
% {strains, stresses, principal stresses, effective stress}
switch (type)
case 1
    e0 = alpha*deltaT*[1; 1; 0];
    c = e/(1 - nu^2)*[1, nu, 0; nu, 1, 0; 0, 0, (1 - nu)/2];
case 2
    e0 = (1 + nu)*alpha*deltaT*[1; 1; 0];
    c = e/((1 + nu)*(1 - 2*nu))*[1 - nu, nu, 0; nu, 1 - nu, 0;
        0, 0, (1 - 2*nu)/2];
end
s = 0; t = 0;
n = [(1/4)*(1 - s)*(1 - t), (1/4)*(s + 1)*(1 -t), ...
        (1/4)*(s + 1)*(t + 1), (1/4)*(1 - s)*(t + 1)];
dns=[(-1 + t)/4, (1 - t)/4, (1 + t)/4, (-1 - t)/4];
dnt=[(-1 + s)/4, (-1 - s)/4, (1 + s)/4, (1 - s)/4];
x = n*coord(:,1); y = n*coord(:,2);
dxs = dns*coord(:,1); dxt = dnt*coord(:,1);
dys = dns*coord(:,2); dyt = dnt*coord(:,2);
J = [dxs, dxt; dys, dyt]; detJ = det(J);
dnx = (J(2, 2)*dns - J(2, 1)*dnt)/detJ;
dny = (-J(1, 2)*dns + J(1, 1)*dnt)/detJ;
b = [dnx(1), 0, dnx(2), 0, dnx(3), 0, dnx(4), 0; 
    0, dny(1), 0, dny(2), 0, dny(3), 0, dny(4);
    dny(1), dnx(1), dny(2), dnx(2), dny(3), dnx(3), dny(4), dnx(4)];
eps = b*dn;
sig = c*(eps-e0)
sx = sig(1); sy= sig(2); sxy=sig(3);
PrincipalStresses = eig([sx,sxy; sxy,sy])
se = sqrt((sx - sy)^2 + sy^2 + sx^2 + 6*sxy^2)/sqrt(2); 