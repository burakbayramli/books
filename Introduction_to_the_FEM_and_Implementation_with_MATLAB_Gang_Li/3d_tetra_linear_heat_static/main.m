% next 5 lines: read the input files
load modelOptions.dat;
load nodes.dat;        
load elements.dat;
load materials.dat;
load nodalTemp.dat;
n_nodes=size(nodes,1);           % number of nodes
n_nodalTemp=size(nodalTemp,1);
%-- Global material properties and constants
kappa=materials(1,1);
convection_coeffcient=materials(2,1);
K=CompK(nodes, elements, kappa);       % compute global K matrix
heatSource=[];                         % there is no heat source
F=CompF(nodes, elements, heatSource);  % compute global F vector

%-- apply temperature boundary condition
coeff=abs(max(K(nodalTemp(1,1),:)))*1e7;  %penalty factor
for i=1:n_nodalTemp
  node_id=nodalTemp(i,1);
  K(node_id, node_id)=coeff;
  F(node_id, 1) = nodalTemp(i,2)*coeff;
end
%-- solve the global linear system
T=K\F;  
%-- save the displacement results in file
TOut=zeros(n_nodes,5);
for n=1:n_nodes
    TOut(n,1:4)=nodes(n,1:4);
    TOut(n,5)=T(n,1);
end
TOut(n_nodes-10:n_nodes,:)

figure (1);
load mymap.dat;
colormap(mymap);
scatter3(nodes(:,2), nodes(:,3),nodes(:,4),600,T(:,1),'filled');