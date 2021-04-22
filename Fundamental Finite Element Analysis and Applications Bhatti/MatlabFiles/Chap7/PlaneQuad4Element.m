function [k, r] = PlaneQuad4Element(type, e, nu, h, alpha, deltaT, bx, by, coord)
% [k, r] = PlaneQuad4Element(e, nu, h, alpha, deltaT, bx, by, coord)
% Generates for a triangular element for plane stress or plane strain problem
% e = Modulus of elasticity
% nu = Poisson's ratio
% h = Thickness
% alpha = coefficient of thermal expansion
% deltaT = temperature change
% bx, by = components of the body force
% coord = coordinates at the element ends

switch (type)
case 1
    e0 = alpha*deltaT*[1; 1; 0];
    c = e/(1 - nu^2)*[1, nu, 0; nu, 1, 0; 0, 0, (1 - nu)/2];
case 2
    e0 = (1 + nu)*alpha*deltaT*[1; 1; 0];
    c = e/((1 + nu)*(1 - 2*nu))*[1 - nu, nu, 0; nu, 1 - nu, 0;
        0, 0, (1 - 2*nu)/2];
end

% Use 2x2 integration. Gauss point locations and weights
pt=1/sqrt(3);
gpLocs = [-pt,-pt; -pt,pt; pt,-pt; pt,pt];
gpWts = [1,1,1,1];
k=zeros(8); r=zeros(8,1);
for i=1:length(gpWts)
    s = gpLocs(i, 1); t = gpLocs(i, 2); w = gpWts(i);
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
    n = [n(1),0,n(2),0,n(3),0,n(4),0;
        0,n(1),0,n(2),0,n(3),0,n(4)];
    k = k + h*detJ*w* b'*c*b;
    r = r + h*detJ*w*n'*[bx;by]+ h*detJ*w*b'*c*e0;
end