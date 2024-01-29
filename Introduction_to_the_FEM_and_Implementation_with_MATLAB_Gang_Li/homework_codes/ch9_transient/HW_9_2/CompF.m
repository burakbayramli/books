function F=CompF(nodes, elements, edgeFlux)
% next 5 lines: set up constants and empty matices
n_nodes = size(nodes,1);
n_elements = size(elements,1);
n_flux_edges = size(edgeFlux,1);
n_nodes_per_element = size(elements,2)-1;
F=zeros(n_nodes,1);
% next 3 lines: get Gauss points, weights and compute shape functions
[gauss_points, gauss_weights]=GetTriEdgeGauss();
n_edge_gauss_points=size(gauss_points,1)/3;
[N,Nx,Ny]=CompNDNatPointsTri6(gauss_points(:,1), gauss_points(:,2));

% for-loop: loop over the number of edges affected by traction
% to compute the element f vector and assemble the global F
for t=1:n_flux_edges
    fe=zeros(n_nodes_per_element, 1);
    eid=edgeFlux(t,1);
    [element_nodes, node_id_map]=SetElementNodes(eid,nodes,elements);
    edge=edgeFlux(t,2);
    for g=1:2
      gid=2*(edge-1)+g;
      J= CompJacobian2DatPoint(element_nodes, Nx(:,gid), Ny(:,gid));
      if  edge==1 
        lengthJ=sqrt(J(1,1)^2+J(1,2)^2);
      elseif edge==3 
        lengthJ=sqrt(J(2,1)^2+J(2,2)^2);
      else
        lengthJ=norm(J(1,:) - J(2,:))/sqrt(2.0);
      end
      Nv=N(:,gid);
      fe =fe+ Nv*edgeFlux(t,5)*lengthJ*gauss_weights(gid);
    end
    F=AssembleGlobalVector(F,fe,node_id_map,1);    % Assemble F
end