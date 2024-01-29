% Compute global force vector
function F=CompF(nodes, elements, thickness, bcsforce)
F=zeros(size(nodes,1)*(size(nodes,2)-1),1);
n_force_nodes = size(bcsforce,1);
% for-loop: apply point forces 
for i=1:n_force_nodes
  row=2*bcsforce(i,1)-1;
  F(row,1) = F(row,1)+ bcsforce(i,2)/thickness;
  F(row+1,1) = F(row+1,1)+ bcsforce(i,3)/thickness;
end