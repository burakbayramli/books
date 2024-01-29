function K=CompK(nodes, elements, materials)  
  n_nodes = size(nodes,1);
  n_elements = size(elements,1);
  n_nodes_per_element = size(elements,2)-1;
  K=zeros(n_nodes*2,n_nodes*2);
  C=CompCPlaneStress(materials);       
  H=CompH();
  [gauss_points, gauss_weights]=GetQuadGauss(2,2);
  n_gauss_points=size(gauss_points,1);
  [N,Nx,Ny]=CompNDNatPointsQuad4(gauss_points(:,1), gauss_points(:,2));
  
  % for-loop: compute K matrix
  for e=1:n_elements    % loop over all the elements 
    ke=zeros(n_nodes_per_element*2, n_nodes_per_element*2);
    [element_nodes, node_id_map]= SetElementNodes(e, nodes, elements);
    % for-loop: compute element matrices 
    for g=1:n_gauss_points
      J=CompJacobian2DatPoint(element_nodes, Nx(:,g), Ny(:,g));
      detJ=det(J);
      Jinv=inv(J);
      Jb(1:2,1:2)=Jinv;
      Jb(3:4,3:4)=Jinv;
      B=CompB4x8Quad4atPoint(Nx(:,g), Ny(:,g));
      HJB=(H*Jb*B);
      ke=ke+HJB'*C*HJB*detJ*gauss_weights(g);
    end
    K= AssembleGlobalMatrix(K,ke,node_id_map,2); % assemble global K
  end  