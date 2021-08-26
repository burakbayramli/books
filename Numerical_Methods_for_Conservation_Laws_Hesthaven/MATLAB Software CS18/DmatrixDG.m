function [D] = DmatrixDG(m,r,V)
% function [D] = DmatrixDG(m,r,V)
% Purpose : Initialize the (r) differentiation matrices, 
% evaluated at (r) at order m
Vr = GradVandermondeDG(m, r); D = Vr/V;
return
