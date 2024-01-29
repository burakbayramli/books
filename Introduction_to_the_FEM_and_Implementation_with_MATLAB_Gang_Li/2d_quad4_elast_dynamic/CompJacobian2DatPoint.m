% Compute Jacobian matrix at a point (xi, eta) in a 2-D master element
% Input: element_nodes: the physical coordinates of a 2-D element
%        in the format of [x1, y1; x2 y2; x3, y3; ...]
% Input: Nx, Ny: dN/dxi, dN/deta vector at the point
% Output: Jacobian matrix at the point
function J= CompJacobian2DatPoint(element_nodes, Nxi, Neta)
J=zeros(2,2);
for j=1:2
  J(1,j) =  Nxi' * element_nodes(:,j);
  J(2,j) =  Neta' * element_nodes(:,j);
end    