clear all;

nx=30;  ny=30; % input: mesh size

% next block: create mesh
[nodes elements]=LShapeUniformMeshQuad4(0,0,.3,.3,nx,ny,-0.1, -0.1, 0.1, 0.1);
n_nodes=size(nodes,1);           % number of nodes
n_elements=size(elements,1);     % number of elements

% next block: set inner wall temperature 
k=1;
for i=1:n_nodes
  if nodes(i,2)<=0.1 && nodes(i,3)==0.1
    nodalTemp(k,:)=[i 140];
    k=k+1;
  elseif nodes(i,2)==0.1 && nodes(i,3)<=0.1
    nodalTemp(k,:)=[i 140];
    k=k+1;
  end
end

% next block: set up the edges that have convection BC
k=1;
for e=1:n_elements
  for j=1:4
    if nodes(elements(e,j+1),3)== 0.3 && nodes(elements(e,rem(j,4)+2),3)== 0.3
      convEdges(k,1:2)=[e j];
      k=k+1;
    elseif nodes(elements(e,j+1),2)== 0.3 && nodes(elements(e,rem(j,4)+2),2)== 0.3
      convEdges(k,1:2)=[e j];
      k=k+1;       
    end
  end
end

% next block: set up the thermal conductivity
kappa=ones(n_elements,1)*0.9;
for e=1:n_elements
  xc=0;
  yc=0;
  for j=1:4
    xc=xc+ nodes(elements(e,j+1),2);
    yc=yc+ nodes(elements(e,j+1),3);
  end
  xc=xc/4;
  yc=yc/4;
  if xc<0.2 && yc<0.2 
    kappa(e,1)=2;
  end
end

n_nodalTemp=size(nodalTemp,1);   % number of nodal temperature

% compute K and F
K=CompK(nodes, elements, kappa, 0);   % compute global K matrix
[K F]=CompEdgeConvectionF(nodes, elements, kappa, 60, 10, convEdges, K); 

% next block: apply temperature boundary condition
coeff=abs(max(K(nodalTemp(1,1),:)))*1e7;  %penalty factor
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
PlotResultsSurfGrid(TOut, 0,0,.3,.3,nx,ny);