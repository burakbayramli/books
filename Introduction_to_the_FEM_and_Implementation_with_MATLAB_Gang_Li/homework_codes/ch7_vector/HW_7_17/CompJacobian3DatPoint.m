% Compute Jacobian matrix at (xi, eta, zeta) in a 3-D master element
% Input: element_nodes: the physical coordinates of a 3-D element
%        in the format of [x1, y1, z1; x2 y2 z2; x3, y3 z3; ...]
% Input: Nxi, Neta, Nzeta: dN/dxi, dN/deta, dN/dzeta vectors at point
% Output: Jacobian matrix at the point
function J= CompJacobian3DatPoint(element_nodes, Nxi, Neta, Nzeta)
J=zeros(3,3);
for j=1:3
  J(1,j) =  Nxi' * element_nodes(:,j);
  J(2,j) =  Neta' * element_nodes(:,j);
  J(3,j) =  Nzeta' * element_nodes(:,j);
end   