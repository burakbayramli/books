function [F]= AssembleGlobalVector(F,fe,node_id_map,ndDOF)

n_nodes_per_element = size(node_id_map,1);

for i = 1:n_nodes_per_element    
  row_node = node_id_map(i,1);
  row=ndDOF*row_node - (ndDOF-1);
  F(row:row+ndDOF-1,1) = F(row:row+ndDOF-1,1) + ...
      fe((i-1)*ndDOF+1:i*ndDOF,1);
end
