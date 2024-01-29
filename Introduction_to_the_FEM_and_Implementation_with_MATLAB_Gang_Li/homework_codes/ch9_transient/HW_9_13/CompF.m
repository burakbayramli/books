% Compute global force vector
function F=CompF(nodes, elements, bcsforce)
F=zeros(size(nodes,1),1);
n_force_nodes = size(bcsforce,1);
% for-loop: apply point forces 
for i=1:n_force_nodes
  row=bcsforce(i,1);
  F(row,1) = F(row,1)+ bcsforce(i,2);
end