

function J= CompEdgeJacobianQuad4Gauss8x1(element_nodes, g, Nx, Ny)
  J=zeros(2,2);
  for j=1:2
    J(1,j) =  (Nx(:,g))' * element_nodes(:,j);
    J(2,j) =  (Ny(:,g))' * element_nodes(:,j);
  end    
