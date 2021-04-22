function [k, r] = PlaneTriElement(type, e, nu, h, alpha, deltaT, bx, by, coord)
% [k, r] = PlaneTriElement(e, nu, h, alpha, deltaT, bx, by, coord)
% Generates for a triangular element for plane stress or plane strain problem
% e = Modulus of elasticity
% nu = Poisson's ratio
% h = Thickness
% alpha = coefficient of thermal expansion
% deltaT = temperature change
% bx, by = components of the body force
% coord = coordinates at the element ends

x1=coord(1,1); y1=coord(1,2);
x2=coord(2,1); y2=coord(2,2);
x3=coord(3,1); y3=coord(3,2);
b1 = y2 - y3; b2 = y3 - y1; b3 = y1 - y2;
c1 = x3 - x2; c2 = x1 - x3; c3 = x2 - x1;
f1 = x2*y3 - x3*y2; f2 = x3*y1 - x1*y3; f3 = x1*y2 - x2*y1;
A = (f1 + f2 + f3)/2;
switch (type)
case 1
    e0 = alpha*deltaT*[1; 1; 0];
    C = e/(1 - nu^2)*[1, nu, 0; nu, 1, 0; 0, 0, (1 - nu)/2];
case 2
    e0 = (1 + nu)*alpha*deltaT*[1; 1; 0];
    C = e/((1 + nu)*(1 - 2*nu))*[1 - nu, nu, 0; nu, 1 - nu, 0;
        0, 0, (1 - 2*nu)/2];
end
B = [b1, 0, c1; 0, c1, b1; b2, 0, c2; 0, c2, b2; 
    b3, 0, c3; 0, c3, b3]/(2*A);
k = h*A*(B*C*B');
r = h*A*(B*C*e0 + [bx; by; bx; by; bx; by]/3);
