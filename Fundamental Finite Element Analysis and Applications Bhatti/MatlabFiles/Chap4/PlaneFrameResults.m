function [f, bm, V] = PlaneFrameResults(modulus, inertia, A, ...
    qs, qt, coord, dn)
% [f, bm, V] = PlaneFrameResults(modulus, inertia, A, ...
%    qs, qt, coord, dn)
% Generates frame element results
% modulus = modulus of elasticity
% inertia = moment of inertia
% A = area of cross-section
% qs = distributed load along the element axis
% qt = distributed load normal to the element axis
% coord = coordinates at the element ends
% dn = nodal solution
% [f, bm, V] = [axial force, bending moment, shear]

EI=modulus*inertia; EA = modulus*A;
x1=coord(1,1); y1=coord(1,2);
x2=coord(2,1); y2=coord(2,2);
L=sqrt((x2-x1)^2+(y2-y1)^2);
ls=(x2-x1)/L; ms=(y2-y1)/L;

u = dn([1,4]);
v = dn([2,3,5,6]);

f=[]; bm=[]; V=[];
% Change increment to get results at more points
for s=0:L/2:L
    x = x1 + s*ls; y = y1+ s*ms;
    f = [f; [x,y,EA*(u(2)-u(1))/L]];
    dn2 = [(12*s)/L^3 - 6/L^2, (6*s)/L^2 - 4/L, 6/L^2 - (12*s)/L^3, ...
            (6*s)/L^2 - 2/L];
    bm = [bm; [x, y, EI*dn2*v+(qt*(L^2 - 6*s*L + 6*s^2))/(12)]];
    dn3 = [12/L^3, 6/L^2, -(12/L^3), 6/L^2];
    V = [V; [x, y, EI*dn3*v+((qt*(12*s - 6*L))/(12))]];
end