function [ke, kp] = BucklingQuadElement(k, coord)
% [ke, kp] = BucklingQuadElement(k, coord)
% Generates equations for a quadratic element for 1D Buckling
% k = bar stiffness (EI)
% coord = coordinates at the element ends

L=coord(3)-coord(1);
ke = [(7*k)/(3*L), -((8*k)/(3*L)), k/(3*L);
    -((8*k)/(3*L)), (16*k)/(3*L), -((8*k)/(3*L));
    k/(3*L), -((8*k)/(3*L)), (7*k)/(3*L)];
kp = [((2*L)/15), (L/15), -L/30;
    (L/15), ((8*L)/15), (L/15);
    -L/30, (L/15), ((2*L)/15)];
