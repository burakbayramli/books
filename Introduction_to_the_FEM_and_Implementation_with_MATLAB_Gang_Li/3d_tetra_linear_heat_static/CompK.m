% Compute global K matrix
function K=CompK(nodes, elements, kappa)
  n_nodes = size(nodes,1);
  n_elements = size(elements,1);
  n_nodes_per_element = size(elements,2)-1;
  K=zeros(n_nodes,n_nodes);
  DN=zeros(3,n_nodes_per_element);
  [gauss_points, gauss_weights]=GetTetraGauss(1);
  n_gauss_points=size(gauss_points,1)
  [N,Nx,Ny,Nz]=CompNDNatPointsTetra4(gauss_points(:,1),...
                    gauss_points(:,2), gauss_points(:,3));
 
  %-- Compute K matrix: loop over all the elements 
  for e=1:n_elements
    ke=zeros(n_nodes_per_element, n_nodes_per_element);
    [element_nodes, node_id_map]= SetElementNodes(e, nodes, elements);
    %-- compute element conductivity matrix ke   
    for g=1:n_gauss_points
      J=CompJacobian3DatPoint(element_nodes,Nx(:,g),Ny(:,g),Nz(:,g));
      detJ=det(J);
      Jinv=inv(J);
      DN(1,:)=Nx(:,g);
      DN(2,:)=Ny(:,g);
      DN(3,:)=Nz(:,g);
      ke=ke+DN'*Jinv'*kappa*Jinv*DN*detJ*gauss_weights(g);
    end
    %-- assemble ke into global K
    K= AssembleGlobalMatrix(K,ke,node_id_map,1);
  end