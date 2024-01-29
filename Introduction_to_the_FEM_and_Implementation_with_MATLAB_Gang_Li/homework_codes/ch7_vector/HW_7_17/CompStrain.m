function Ep=CompStrain(nodes, elements, materials, Usol)
n_nodes = size(nodes,1);
n_elements = size(elements,1);
n_element_nodes = size(elements,2)-1;
Ep=zeros(n_nodes,7);

master_nodes=[-1 -1 -1; 1 -1 -1; 1 1 -1; -1 1 -1;...
              -1 -1 1; 1 -1 1; 1 1 1; -1 1 1];
[N,Nx,Ny,Nz]=CompNDNatPointsHexa8(master_nodes(:,1), master_nodes(:,2),...
                                  master_nodes(:,3));

% for-loop block: loop over all the elements 
for e=1:n_elements
  Ue(1:3:3*n_element_nodes-2,1)=Usol(elements(e,2:1+n_element_nodes)*3-2);
  Ue(2:3:3*n_element_nodes-1,1)=Usol(elements(e,2:1+n_element_nodes)*3-1);
  Ue(3:3:3*n_element_nodes,1)=Usol(elements(e,2:1+n_element_nodes)*3);
  [element_nodes, node_id_map]= SetElementNodes(e, nodes, elements);
  
  %-- compute strains at the element nodes
  for v=1:n_element_nodes
    node_v=elements(e,1+v);
    J=CompJacobian3DatPoint(element_nodes, Nx(:,v), Ny(:,v), Nz(:,v));
    detJ=det(J);
    Jinv=inv(J);
    B=CompB6x24Hexa8atPoint(Nx(:,v), Ny(:,v), Nz(:,v), Jinv);
    ep=B*Ue;
    Ep(node_v,1)=Ep(node_v,1)+1;
    Ep(node_v,2:7)=Ep(node_v,2:7) + ep(1:6,1)';
  end
end
Ep(:,2:7)=Ep(:,2:7)./Ep(:,1);  % averaging the strains at the nodes
Ep(:,1)=[1:n_nodes]';