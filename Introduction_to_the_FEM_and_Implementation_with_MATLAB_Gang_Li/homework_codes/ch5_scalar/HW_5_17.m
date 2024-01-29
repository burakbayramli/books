clear all;
% next block: create mesh
UniformMeshQuad4(0,0,4,8,16,32);
load nodes.dat;
load elements.dat;
n_nodes=size(nodes,1);           % number of nodes
n_elements=size(elements,1);     % number of elements

% next block: set wire temperature 
for i=1:n_nodes
  if nodes(i,2)==2 && nodes(i,3)==6
    nodalTemp=[i 80];
  end
end

% next block: set up the edges that have convection BC
k=1;
for e=1:n_elements
  for j=1:4
    if nodes(elements(e,j+1),3)== 8 && nodes(elements(e,rem(j,4)+2),3)== 8
      convEdges(k,1:2)=[e j];
      k=k+1;
    end
  end
end

n_nodalTemp=size(nodalTemp,1);   % number of nodal temperature
% global material properties and constants
kappa=ones(n_elements,1)*10;
% compute K and F
K=CompK(nodes, elements, kappa, 0);   % compute global K matrix
[K F]=CompEdgeConvectionF(nodes, elements, kappa, 5, -5, convEdges, K); 
% apply temperature boundary condition
coeff=abs(max(K(nodalTemp(1,1),:)))*1e6;  %penalty factor
for i=1:n_nodalTemp
  node_id=nodalTemp(i,1);
  K(node_id, node_id)=coeff;
  F(node_id, 1) = nodalTemp(i,2)*coeff;
end
% solve the global linear system
T=K\F;  
% save the displacement results in file
TOut=zeros(n_nodes,4);
for n=1:n_nodes
    TOut(n,1:3)=nodes(n,1:3);
    TOut(n,4)=T(n,1);
end
PlotResultsSurfGrid(TOut, 0,0,4,8,16,32);