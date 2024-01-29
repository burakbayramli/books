% Compute F vector
% Input: nodes, elements, kappa (thermal conducticity), 
%        hz (surface convection related parameter), 
%        extTemp (environmental temperatre), edgeFlux (flux on edges)
% Output: F vector
function [K F]=CompEdgeConvectionF(nodes, elements, kappa, h, extTemp, convEdges, K)
n_nodes = size(nodes,1);
n_elements = size(elements,1);
n_conv_edges = size(convEdges,1);
n_nodes_per_element = size(elements,2)-1;
F=zeros(n_nodes,1);

% next 23 lines: apply edge flux
n_edge_gauss_points=2;
[gauss_points,gauss_weights]=GetQuadEdgeGauss(n_edge_gauss_points);
[N,Nx,Ny]=CompNDNatPointsQuad4(gauss_points(:,1), gauss_points(:,2));
% for-loop block: loop over the edges affected by flux and apply flux
for t=1:n_conv_edges
    fe=zeros(n_nodes_per_element, 1);
    ke=zeros(n_nodes_per_element,n_nodes_per_element);
    eid=convEdges(t,1);
    [element_nodes,node_id_map]= SetElementNodes(eid,nodes,elements);
    edge=convEdges(t,2);
    % loop over the edge Gauss points to compute boundary integral
    for g=1:n_edge_gauss_points
      gid=2*edge-2+g;   % Gauss point ID in edge Gauss points
      J=CompJacobian2DatPoint(element_nodes,Nx(:,gid),Ny(:,gid));
      if  (edge==1) | (edge==3)  
        lengthJ=sqrt(J(1,1)^2+J(1,2)^2);
      else 
        lengthJ=sqrt(J(2,1)^2+J(2,2)^2);
      end
      Nv=N(:,gid);
      fe =fe+ Nv*h*extTemp*lengthJ*gauss_weights(gid);
      ke=ke+ Nv*Nv'*h*lengthJ*gauss_weights(gid);
    end
    F= AssembleGlobalVector(F,fe,node_id_map,1); % assemble F
    K= AssembleGlobalMatrix(K,ke,node_id_map,1);
end
