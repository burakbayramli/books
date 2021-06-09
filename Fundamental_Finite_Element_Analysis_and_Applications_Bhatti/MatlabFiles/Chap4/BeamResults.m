function [v, bm, V] = BeamResults(EI, q, coord, dn)
% [v, bm, V] = BeamResults(EI, q, coord, dn)
% Generates beam element results
% EI = beam stiffness
% q = distributed load
% coord = coordinates at the element ends
% dn = nodal solution
L=coord(2)-coord(1);
v=[]; bm=[]; V=[];
% Change increment to get results at more points
for s=0:L/2:L
    n = [(2*s^3)/L^3 - (3*s^2)/L^2 + 1, s^3/L^2 - (2*s^2)/L + s, ...
            (3*s^2)/L^2 - (2*s^3)/L^3, s^3/L^2 - s^2/L];
    v = [v; [coord(1)+s, n*dn+(q*(L - s)^2*s^2)/(24*EI)]];
    dn2 = [(12*s)/L^3 - 6/L^2, (6*s)/L^2 - 4/L, 6/L^2 - (12*s)/L^3, ...
            (6*s)/L^2 - 2/L];
    bm = [bm; [coord(1)+s, EI*dn2*dn+(q*(L^2 - 6*s*L + 6*s^2))/(12)]];
    dn3 = [12/L^3, 6/L^2, -(12/L^3), 6/L^2];
    V = [V; [coord(1)+s, EI*dn3*dn+((q*(12*s - 6*L))/(12))]];
end