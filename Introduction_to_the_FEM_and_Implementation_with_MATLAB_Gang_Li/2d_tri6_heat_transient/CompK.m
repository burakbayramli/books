function K=CompK(nodes, elements, kappa)
% next 5 lines: set up constants and empty matices
n_nodes = size(nodes,1);
n_elements = size(elements,1);
n_nodes_per_element = size(elements,2)-1;
K=zeros(n_nodes,n_nodes);
DN=zeros(2,6);
% get Gauss points, weights and compute shape functions
[gauss_points, gauss_weights]=GetTriGauss();
n_gauss_points=size(gauss_points,1);
[N,Nx,Ny]=CompNDNatPointsTri6(gauss_points(:,1), gauss_points(:,2));

% for-loop: compute K matrix 
for e=1:n_elements     % loop over all the elements 
  ke=zeros(n_nodes_per_element, n_nodes_per_element);
  [element_nodes, node_id_map]= SetElementNodes(e,nodes, elements);
  for g=1:n_gauss_points
    J= CompJacobian2DatPoint(element_nodes, Nx(:,g), Ny(:,g));
    detJ=det(J);
    Jinv=inv(J);
    DN(1,:)=Nx(:,g);
    DN(2,:)=Ny(:,g);
    ke=ke+DN'*Jinv'*kappa*Jinv*DN*detJ*gauss_weights(g);
  end
  K= AssembleGlobalMatrix(K,ke,node_id_map,1); % assemble global K
end