% next block: set up model
nodes=[1 0 10; 2 0 4; 3 0 0; 4 6 10; 5 5 7; 6 6.5 0; 7 10 10; 8 10 5; 9 10 0];
elements=[1 1 2 5 4; 2 2 3 6 5; 3 4 5 8 7; 4 5 6 9 8];
nodalTemp=[1 1; 2 1; 3 1];
edgeFlux=[3 3 -1; 4 3 -1];
n_nodes=size(nodes,1);           % number of nodes
n_nodalTemp=size(nodalTemp,1);   % number of nodal temperature

kappa=ones(4,1);  % thermal conductivity

% next block: compute K and F
K=CompK(nodes, elements, kappa, 0);   % compute global K matrix
F=CompF(nodes, elements, kappa, 0, 0, edgeFlux); % global F

% next block: apply temperature boundary condition
coeff=abs(max(K(nodalTemp(1,1),:)))*1e7;  %penalty factor
for i=1:n_nodalTemp
  node_id=nodalTemp(i,1);
  K(node_id, node_id)=coeff;
  F(node_id, 1) = nodalTemp(i,2)*coeff;
end

% solve the global linear system
T=K\F;  

% next block: save the displacement results in file
TOut=zeros(n_nodes,4);
for n=1:n_nodes
    TOut(n,1:3)=nodes(n,1:3);
    TOut(n,4)=T(n,1);
end
TOut

[Txy]=CompFlux(nodes, elements, kappa, T); % compute heat flux
Txy