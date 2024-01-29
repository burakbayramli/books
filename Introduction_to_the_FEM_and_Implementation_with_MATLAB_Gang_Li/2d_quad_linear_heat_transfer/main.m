% next 6 lines: read the input files
load options.dat;
load nodes.dat;        
load elements.dat;
load materials.dat;
load edgeFlux.dat;
load nodalTemp.dat;
n_nodes=size(nodes,1);           % number of nodes
n_nodalTemp=size(nodalTemp,1);   % number of nodal temperature
% global material properties and constants
kappa=materials(3,1);
thickness=options(2,1);
convection_coeffcient=materials(4,1);
hz=convection_coeffcient*2/thickness;
extTemp=options(4,1);
% compute K and F
K=CompK(nodes, elements, kappa, hz);   % compute global K matrix
F=CompF(nodes, elements, kappa, hz, extTemp, edgeFlux); % global F
% apply displacement boundary condition
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
TOut
save -ascii -double feT.dat TOut;
plotSurfGridMesh