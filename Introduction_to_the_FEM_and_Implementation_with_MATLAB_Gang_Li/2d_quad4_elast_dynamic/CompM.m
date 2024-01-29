% Compute mass matrix
function M=CompM(nodes, elements, rho)
n_nodes = size(nodes,1);
n_elements = size(elements,1);
n_nodes_per_element = size(elements,2)-1;
M=zeros(n_nodes*2,n_nodes*2);
Nv=zeros(8,2);
[gauss_points, gauss_weights]=GetQuadGauss(2,2);
[N,Nx,Ny]=CompNDNatPointsQuad4(gauss_points(:,1),gauss_points(:,2));
  
% for-loop: compute M matrix: loop over all the elements 
for e=1:n_elements
  me=zeros(n_nodes_per_element*2, n_nodes_per_element*2);
  [element_nodes, node_id_map]= SetElementNodes(e, nodes, elements);
  % for-loop: compute element mass matrix me
  for g=1:size(gauss_points,1)
    J=CompJacobian2DatPoint(element_nodes, Nx(:,g), Ny(:,g));
    detJ=det(J);
    for p=1:4 
      Nv(2*p-1,1)=N(p,g);
      Nv(2*p,2)=N(p,g);
    end
    me=me+Nv*Nv'*detJ*rho*gauss_weights(g);
  end
  M= AssembleGlobalMatrix(M,me,node_id_map,2); % assemble global M
end