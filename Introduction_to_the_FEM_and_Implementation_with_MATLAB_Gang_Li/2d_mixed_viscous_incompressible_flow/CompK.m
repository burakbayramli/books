function K=CompK(nodes, pnodes, elements, pelements, materials)  
n_nodes = size(nodes,1);
n_pnodes=max(pnodes(:,2));
n_elements = size(elements,1);
n_nodes_per_element = size(elements,2)-1;
K=zeros(n_nodes*2+n_pnodes,n_nodes*2+n_pnodes);
C=materials(1)*[2 0 0; 0 2 0; 0 0 1];       
H=CompH();
[gauss_points, gauss_weights]=GetQuadGauss(3,3);
n_gauss_points=size(gauss_points,1);
[N,Nx,Ny]=CompNDNatPointsQuad8(gauss_points(:,1),gauss_points(:,2));

% for-loop: compute K11 block of K
for e=1:n_elements    % loop over all the elements 
  ke=zeros(n_nodes_per_element*2, n_nodes_per_element*2);
  [element_nodes, node_id_map]= SetElementNodes(e, nodes, elements);
  % for-loop: compute element stiffness matrix ke     
  for g=1:n_gauss_points
    J=CompJacobian2DatPoint(element_nodes,Nx(:,g),Ny(:,g));
    detJ=det(J);
    Jinv=inv(J);
    Jb(1:2,1:2)=Jinv;
    Jb(3:4,3:4)=Jinv;
    B=CompBQuadGaussatPoint(n_nodes_per_element, Nx(:,g), Ny(:,g));
    HJB=(H*Jb*B);
    ke=ke+HJB'*C*HJB*detJ*gauss_weights(g);
  end  
  K= AssembleGlobalMatrix(K,ke,node_id_map,2); % assemble global K
end  

%--- setup for K12 block
n_pnodes_per_element = size(pelements,2)-1;
K12=zeros(n_nodes*2,n_pnodes);
[NP,NPx,NPy]=CompNDNatPointsQuad4(gauss_points(:,1), gauss_points(:,2));
% for-loop: compute K12 block of K
for e=1:n_elements
  ke=zeros(n_nodes_per_element*2, n_pnodes_per_element);
  [element_nodes, node_id_map]=SetElementNodes(e, nodes, elements);
  [pelement_nodes, pnode_id_map]=SetElementNodes(e,nodes,pelements);
  % for-loop: compute element stiffness matrix ke     
  for g=1:n_gauss_points
    J=CompJacobian2DatPoint(element_nodes,Nx(:,g),Ny(:,g));
    detJ=det(J);
    Jinv=inv(J);
    dN=zeros(2*n_nodes_per_element,1);
    for j=1: n_nodes_per_element
       dN(2*j-1:2*j,1)= Jinv*[Nx(j,g) Ny(j,g)]';
    end
    ke=ke+dN*(NP(:,g))'*detJ*gauss_weights(g);
  end  
  % for-loop: assemble ke into global KP
  for i = 1:n_nodes_per_element    
    row_node = node_id_map(i,1);
    row=2*row_node - 1;
    for j = 1:n_pnodes_per_element 
      col_node = pnode_id_map(j,1);
      pnode_id=pnodes(col_node,2);
      col=pnode_id;
      K12(row:row+1, col)=K12(row:row+1, col) + ke(2*i-1:i*2, j);
    end
  end
end  
% next 2 lines: fill in the symmetric part of K
K(1:2*n_nodes, 2*n_nodes+1:2*n_nodes+n_pnodes)=-K12;
K(2*n_nodes+1:2*n_nodes+n_pnodes, 1:2*n_nodes)=-K12';