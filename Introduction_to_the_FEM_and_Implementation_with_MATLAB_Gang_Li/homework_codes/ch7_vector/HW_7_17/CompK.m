function K=CompK(nodes, elements, materials)
n_nodes = size(nodes,1);
n_elements = size(elements,1);
n_nodes_per_element = size(elements,2)-1;
K=zeros(n_nodes*3,n_nodes*3);
C=CompC3D(materials);     
[gauss_points, gauss_weights]=GetHexaGauss(2,2,2);
n_gauss_points=size(gauss_points,1);
[N,Nx,Ny,Nz]=CompNDNatPointsHexa8(gauss_points(:,1), ...
                   gauss_points(:,2), gauss_points(:,3));
 
% compute K matrix: loop over all the elements 
for e=1:n_elements
  ke=zeros(n_nodes_per_element*3, n_nodes_per_element*3);
  [element_nodes, node_id_map]= SetElementNodes(e, nodes, elements);
  % next 7 lines: compute element stiffness matrix ke   
  for g=1:n_gauss_points
    J=CompJacobian3DatPoint(element_nodes, Nx(:,g), Ny(:,g), Nz(:,g));
    detJ=det(J);
    Jinv=inv(J);
    %Nx(:,g)
    %Ny(:,g)
    %Nz(:,g)
    B=CompB6x24Hexa8atPoint(Nx(:,g), Ny(:,g), Nz(:,g), Jinv);
    ke=ke+B'*C*B*detJ*gauss_weights(g);
  end
  % assemble ke into global K
  K= AssembleGlobalMatrix(K,ke,node_id_map,3);
end