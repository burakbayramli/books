% Compute stresses
% Input: nodes, elements, E, nu,, 
%        Usol (computed nodal displacement)
% Output: nodal Sxy vectors (stresses)
function Sxy=CompStress(nodes, elements, materials, Usol)
n_nodes = size(nodes,1);
n_elements = size(elements,1);
n_element_nodes = size(elements,2)-1;
Sxy=zeros(n_nodes,5);
C=CompCPlaneStress(materials);       % compute material matrix
H=CompH(); 

if n_element_nodes==4
  master_nodes=[1 1; -1 1; -1 -1; 1 -1];
  [N,Nx,Ny]=CompNDNatPointsQuad4(master_nodes(:,1), master_nodes(:,2));
elseif n_element_nodes==3
  master_nodes=[0 0; 1 0; 0 1];
  [N,Nx,Ny]=CompNDNatPointsTri3(master_nodes(:,1), master_nodes(:,2));
else
  fprintf('Error computing stresses\n'); % error message 
  return;
end

% for-loop block: compute heat fluxes: loop over all the elements 
for e=1:n_elements
  Ue(1:2:2*n_element_nodes-1,1)=Usol(elements(e,2:1+n_element_nodes)*2-1);
  Ue(2:2:2*n_element_nodes,1)=Usol(elements(e,2:1+n_element_nodes)*2);
  [element_nodes, node_id_map]= SetElementNodes(e, nodes, elements);
  %-- compute flux at the element nodes
  for v=1:n_element_nodes
    node_v=elements(e,1+v);
    J=CompJacobian2DatPoint(element_nodes, Nx(:,v), Ny(:,v));
    detJ=det(J);
    Jinv=inv(J);
    Jb(1:2,1:2)=Jinv;
    Jb(3:4,3:4)=Jinv;
    if n_element_nodes==4
      B=CompB4x8Quad4atPoint(Nx(:,v), Ny(:,v));
    elseif n_element_nodes==3
      B=CompB4x6Tri3atPoint(Nx(:,v), Ny(:,v));
    end
    stress=C*H*Jb*B*Ue;
    Sxy(node_v,1)=Sxy(node_v,1)+1;
    Sxy(node_v,2)=Sxy(node_v,2)+stress(1);
    Sxy(node_v,3)=Sxy(node_v,3)+stress(2);
    Sxy(node_v,4)=Sxy(node_v,4)+stress(3);
    % next line: von Mises stress
    Sv=sqrt(stress(1)^2 +  stress(2)^2 - stress(1)*stress(2) + 3*stress(3)^2);
    Sxy(node_v,5)=Sxy(node_v,5)+Sv;
  end
end

Sxy(:,2:5)=Sxy(:,2:5)./Sxy(:,1);  % averaging the stresses at the nodes
Sxy(:,1)=[1:n_nodes]';