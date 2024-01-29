%-----------------------------------------------------
% Compute Jacobian at a given Gauss point 
% Input:
%   element_nodes: nodal coordinates of the element
%   g: g-th Gauss point for which the Jacobian is
%      computed
% Output:
%   J: Jacobian matrix at the g-th Gauss point 
%----------------------------------------------------

function J= CompJacobian2DatGaussPt(element_nodes, g, Nx, Ny)
  J=zeros(2,2);
  for j=1:2
    J(1,j) =  (Nx(:,g))' * element_nodes(:,j);
    J(2,j) =  (Ny(:,g))' * element_nodes(:,j);
  end    
