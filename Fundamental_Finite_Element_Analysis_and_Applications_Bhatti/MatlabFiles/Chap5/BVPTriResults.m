function results = BVPTriResults(coord, dn)
% results = BVPTriResults(coord, dn)
% Computes element solution for a triangular element for 2D BVP
% coord = nodal coordinates
% dn = nodal solution
% The results are computed at the element center
% The first three output variables are u and its x and y derivatives
% The last output variable is integral of solution over the element
x1=coord(1,1); y1=coord(1,2);
x2=coord(2,1); y2=coord(2,2);
x3=coord(3,1); y3=coord(3,2);
x=(x1+x2+x3)/3; y=(y1+y2+y3)/3;
b1 = y2 - y3; b2 = y3 - y1; b3 = y1 - y2;
c1 = x3 - x2; c2 = x1 - x3; c3 = x2 - x1;
f1 = x2*y3 - x3*y2; f2 = x3*y1 - x1*y3; f3 = x1*y2 - x2*y1;
A = (f1 + f2 + f3)/2;
n = [f1 + x*b1 + y*c1, f2 + x*b2 + y*c2, f3 + x*b3 + y*c3]/(2*A);
u = n*dn; dudx=[b1, b2, b3]*dn/(2*A); dudy=[c1, c2, c3]*dn/(2*A);
ui = sum(dn)*A/3;
results=[u, dudx, dudy, ui];