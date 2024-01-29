% Compute F vector
% Input: nodes, elements, materials, bcstraction, bcsforce
% Output: F vector
function F=CompF(nodes, elements, materials, bcstraction, bcsforce)
n_nodes = size(nodes,1);
n_elements = size(elements,1);
n_traction_edges = size(bcstraction,1);
n_force_nodes = size(bcsforce,1);
n_nodes_per_element = size(elements,2)-1;
traction_start=zeros(2,1);
traction_end=zeros(2,1);
traction=zeros(2,1);
F=zeros(n_nodes*2,1);
n_edge_gauss_points=2;
[gauss_points,gauss_weights]=GetQuadEdgeGauss(n_edge_gauss_points);
edge_gauss=-gauss_points(1:n_edge_gauss_points,1);
[N,Nx,Ny]=CompNDNatPointsQuad4(gauss_points(:,1),gauss_points(:,2));
  
% for-loop: loop over the number of edges affected by traction
% to apply surface traaction
for t=1:n_traction_edges
  fe=zeros(n_nodes_per_element*2, 1);
  eid=bcstraction(t,1);
  [element_nodes, node_id_map]= SetElementNodes(eid,nodes,elements);
  edge=bcstraction(t,2);
  traction_start(1:2,1)=bcstraction(t,5:6);
  traction_end(1:2,1)=bcstraction(t,7:8);
  % loop over the edge Guass points
  for g=1:n_edge_gauss_points
    gid=n_edge_gauss_points*(edge-1)+g;    % Gauss point ID
    J=CompJacobian2DatPoint(element_nodes,Nx(:,gid),Ny(:,gid));
    if  (edge==1) | (edge==3)  
      lengthJ=sqrt(J(1,1)^2+J(1,2)^2);
    else 
      lengthJ=sqrt(J(2,1)^2+J(2,2)^2);
    end
    fxy=zeros(8,2);
    for k=1:n_nodes_per_element   
      fxy(2*k-1,1)=N(k, gid);
      fxy(2*k,2)=N(k, gid);
    end
    traction(1,1)=(1-edge_gauss(g))/2*traction_start(1,1)...
                  +(1+edge_gauss(g))/2*traction_end(1,1);
    traction(2,1)=(1-edge_gauss(g))/2*traction_start(2,1)...
                  +(1+edge_gauss(g))/2*traction_end(2,1);

    fe =fe+ fxy*traction*lengthJ*gauss_weights(gid);
  end
  F= AssembleGlobalVector(F, fe, node_id_map, 2); % assemble F
end
% for loop: apply point forces
for i=1:n_force_nodes
  row=2*bcsforce(i,1)-1;
  F(row,1) = F(row,1)+ bcsforce(i,2)/materials(3); % materials(3) is
  F(row+1,1) = F(row+1,1)+ bcsforce(i,3)/materials(3); % thickness
end