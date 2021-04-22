function [ke, kp] = BucklingLinElement(k, coord)
% [ke, kp] = BucklingLinElement(k, coord)
% Generates equations for a linear element for 1D Buckling
% k = bar stiffness (EI)
% coord = coordinates at the element ends

L=coord(2)-coord(1);
ke = k/L*[1, -1; -1, 1];
kp = [L/3, L/6; L/6, L/3];
