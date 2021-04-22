function k = AxialDefElement(e, A, coord)
% k = AxialDefElement(e, A, coord)
% Generates stiffness matrix of an axial deformation element
% e = modulus of elasticity
% A = Area of cross-section
% coord = coordinates at the element ends

x1=coord(1); x2=coord(2);
k = e*A/(x2-x1)*[1,-1; -1,1];