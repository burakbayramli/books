function results = BVPRectResults(coord, dn)
% results = BVPRectResults(coord, dn)
% Computes element solution for a rectangular element for 2D BVP
% coord = nodal coordinates
% dn = nodal solution
% Output variables are u and its x and y derivatives at element center
x1=coord(1,1); y1=coord(1,2);
x2=coord(2,1); y2=coord(2,2);
x3=coord(3,1); y3=coord(3,2);
x4=coord(4,1); y4=coord(4,2);
s = 0; t = 0;
a = abs((x2 - x1))/2; b = abs((y4 - y2))/2;
u = 1/(4*a*b)*[(a - s)*(b - t), (a + s)*(b - t), ...
        (a + s)*(b + t), (a - s)*(b + t)]*dn;
dudx= 1/(4*a*b)*[-b + t, b - t, b + t, -b - t]*dn;
dudy = 1/(4*a*b)*[-a + s, -a - s, a + s, a - s]*dn;
results=[u, dudx, dudy];