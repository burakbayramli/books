function rq = AxialDefLoad(q, coord)
% rq = AxialDefLoad(q, coord)
% Generates equivalent load vector for an axial deformation element
% q = uniformly distributed load
% coord = coordinates at the element ends

x1=coord(1); x2=coord(2);
rq = q*(x2-x1)/2*[1;1];