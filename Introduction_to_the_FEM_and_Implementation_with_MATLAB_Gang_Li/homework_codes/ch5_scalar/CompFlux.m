% Compute heat flux
% Input: nodes, elements, kappa (thermal conducticity), 
%        Tsol (computed nodal temperature)
% Output: nodal Tx, Ty vectors
function [Txy]=CompFlux(nodes, elements, kappa, Tsol)
n_nodes = size(nodes,1);
n_elements = size(elements,1);
n_element_nodes = size(elements,2)-1;
Txy=zeros(n_nodes,3);
DN=zeros(2,4);

if n_element_nodes==4
  master_nodes=[1 1; -1 1; -1 -1; 1 -1];
else
  fprintf('Error computing flux\n'); % error message 
  return;
end
[N,Nx,Ny]=CompNDNatPointsQuad4(master_nodes(:,1), master_nodes(:,2));

% for-loop block: compute heat fluxes: loop over all the elements 
for e=1:n_elements
  Te=Tsol(elements(e,2:1+n_element_nodes));
  [element_nodes, node_id_map]= SetElementNodes(e, nodes, elements);
  %-- compute flux at the element nodes
  for v=1:n_element_nodes
    node_v=elements(e,1+v);
    J=CompJacobian2DatPoint(element_nodes, Nx(:,v), Ny(:,v));
    Jinv=inv(J);
    DN(1,:)=Nx(:,v);
    DN(2,:)=Ny(:,v);
    flux=kappa(e)*Jinv*DN*Te;
    Txy(node_v,1)=Txy(node_v,1)+1;
    Txy(node_v,2)=Txy(node_v,2)+flux(1);
    Txy(node_v,3)=Txy(node_v,3)+flux(2);
  end
end

Txy(:,2:3)=Txy(:,2:3)./Txy(:,1);  % averaging the flux at the nodes
Txy(:,1)=[1:n_nodes]';