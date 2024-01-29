function F=CompF(nodes, elements, bcsforce)
n_nodes = size(nodes,1);
n_force_nodes = size(bcsforce,1);
F=zeros(n_nodes*3,1);

% for-loop: apply point forces
for i=1:n_force_nodes
  row=3*bcsforce(i,1)-2;
  F(row,1) = F(row,1)+ bcsforce(i,2);
  F(row+1,1) = F(row+1,1)+ bcsforce(i,3);
  F(row+2,1) = F(row+2,1)+ bcsforce(i,4);
end