% Compute global RHS load vector
function F=CompF(nodes, elements, heatSource)
n_nodes = size(nodes,1);
n_heat_nodes = size(heatSource,1);
F=zeros(n_nodes,1);
% for-loop: apply heat source
for i=1:n_heat_nodes
  row=heatSource(i,1);
  F(row,1) = F(row,1)+ heatSource(i,2);
end