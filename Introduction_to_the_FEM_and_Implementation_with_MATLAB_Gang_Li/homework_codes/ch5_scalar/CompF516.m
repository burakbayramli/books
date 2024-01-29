% Compute F vector (modified for Problem 5.16)
% Input: nodes, elements, kappa (thermal conductivity), 
%        hz (surface convection related parameter), 
%        extTemp (environmental temperate), edgeFlux (flux on edges)
% Output: F vector
function F=CompF516(nodes, elements, kappa, edgeFlux)
n_nodes = size(nodes,1);
n_elements = size(elements,1);
n_flux_edges = size(edgeFlux,1);
n_nodes_per_element = size(elements,2)-1;
F=zeros(n_nodes,1);

n_edge_gauss_points=3;
x=[-sqrt(3/5) 0 sqrt(3/5)]';
[gauss_points,gauss_weights]=GetQuadEdgeGauss(n_edge_gauss_points);
if n_nodes_per_element==4
  [N,Nx,Ny]=CompNDNatPointsQuad4(gauss_points(:,1), gauss_points(:,2));
elseif n_nodes_per_element==8
  [N,Nx,Ny]=CompNDNatPointsQuad8(gauss_points(:,1), gauss_points(:,2));
elseif n_nodes_per_element==12
  [N,Nx,Ny]=CompNDNatPointsQuad12(gauss_points(:,1), gauss_points(:,2));
end

% for-loop: loop over the edges affected by flux and apply flux
for t=1:n_flux_edges
    fe=zeros(n_nodes_per_element, 1);
    eid=edgeFlux(t,1);
    [element_nodes,node_id_map]= SetElementNodes(eid,nodes,elements);
    edge=edgeFlux(t,2);
    % loop over the edge Gauss points to compute boundary integral
    for g=1:n_edge_gauss_points
      gid=3*edge-3+g;   % Gauss point ID in edge Gauss points
      J=CompJacobian2DatPoint(element_nodes,Nx(:,gid),Ny(:,gid));
      if  (edge==1) | (edge==3)  
        lengthJ=sqrt(J(1,1)^2+J(1,2)^2);
      else 
        lengthJ=sqrt(J(2,1)^2+J(2,2)^2);
      end
      Nv=N(:,gid);
      if n_nodes_per_element==4
        eflux= edgeFlux(t,3)*(1-x(g))/2 + edgeFlux(t,4)*(1+x(g))/2;
      elseif n_nodes_per_element==8
        eflux= edgeFlux(t,3)*x(g)*(x(g)-1)/2 + ...
               edgeFlux(t,4)*(1-x(g))*(1+x(g)) + ...
               edgeFlux(t,5)*x(g)*(1+x(g))/2;
      elseif n_nodes_per_element==12
        eflux= edgeFlux(t,3)*(x(g)+1/3)*(x(g)-1/3)*(x(g)-1)/(-16/9) + ...
               edgeFlux(t,4)*(x(g)+1)*(x(g)-1/3)*(x(g)-1)/(16/27) + ...
               edgeFlux(t,5)*(x(g)+1)*(x(g)+1/3)*(x(g)-1)/(-16/27)+...
               edgeFlux(t,6)*(x(g)+1)*(x(g)+1/3)*(x(g)-1/3)/(16/9);
      end
      fe =fe+ Nv*eflux*lengthJ*gauss_weights(gid);
    end
    F= AssembleGlobalVector(F,fe,node_id_map,1); % assemble F
end