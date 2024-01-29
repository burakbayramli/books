% Compute damping matrix
function C=CompC(nodes, elements, cv, rho)
% next 5 lines: set up constants and empty matices
n_nodes = size(nodes,1);
n_elements = size(elements,1);
n_nodes_per_element = size(elements,2)-1;
C=zeros(n_nodes,n_nodes);
Nv=zeros(1,6);
% get Gauss points, weights and compute shape functions
[gauss_points, gauss_weights]=GetTriGauss();
n_gauss_points=size(gauss_points,1);
[N,Nx,Ny]=CompNDNatPointsTri6(gauss_points(:,1), gauss_points(:,2));

% for-loop: compute C matrix
for e=1:n_elements    % loop over all the elements 
  ce=zeros(n_nodes_per_element,n_nodes_per_element);
  [element_nodes, node_id_map]=SetElementNodes(e, nodes, elements);
  for g=1:n_gauss_points
    J= CompJacobian2DatPoint(element_nodes, Nx(:,g), Ny(:,g));
    detJ=det(J);
    Jinv=inv(J);
    Nv(1,:)=N(:,g);
    ce=ce+Nv'*Nv*detJ*cv*rho*gauss_weights(g);
  end
  C= AssembleGlobalMatrix(C,ce,node_id_map,1); % assemble global C
end