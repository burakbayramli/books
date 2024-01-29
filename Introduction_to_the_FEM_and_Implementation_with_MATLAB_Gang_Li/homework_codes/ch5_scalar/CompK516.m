% Compute K matrix (modified for Problem 5.16)
% Input: nodes, elements, kappa (thermal conductivity), 
%        hz (surface convection related parameter), 
% Output: K matrix
function K=CompK516(nodes, elements, kappa)
n_nodes = size(nodes,1);
n_elements = size(elements,1);
n_nodes_per_element = size(elements,2)-1;
K=zeros(n_nodes,n_nodes);
DN=zeros(2,n_nodes_per_element);
Nv=zeros(1,n_nodes_per_element);
[gauss_points, gauss_weights]=GetQuadGauss(3,3); % use 3x3 for all cases
n_gauss_points=size(gauss_points,1);
if n_nodes_per_element==4
  [N,Nx,Ny]=CompNDNatPointsQuad4(gauss_points(:,1), gauss_points(:,2));
elseif n_nodes_per_element==8
  [N,Nx,Ny]=CompNDNatPointsQuad8(gauss_points(:,1), gauss_points(:,2));
elseif n_nodes_per_element==12
  [N,Nx,Ny]=CompNDNatPointsQuad12(gauss_points(:,1), gauss_points(:,2));
end

% for-loop block: compute K matrix: loop over all the elements 
for e=1:n_elements
  ke=zeros(n_nodes_per_element, n_nodes_per_element);
  [element_nodes, node_id_map]= SetElementNodes(e, nodes, elements);
  %-- compute element stiffness matrix ke  
  for g=1:n_gauss_points
    J=CompJacobian2DatPoint(element_nodes, Nx(:,g), Ny(:,g));
    detJ=det(J);
    Jinv=inv(J);
    DN(1,:)=Nx(:,g);
    DN(2,:)=Ny(:,g);
    Nv(1,:)=N(:,g);
    ke=ke+DN'*Jinv'*kappa(e)*Jinv*DN*detJ*gauss_weights(g);
  end
  %-- assemble ke into global K
  K= AssembleGlobalMatrix(K,ke,node_id_map,1);
end