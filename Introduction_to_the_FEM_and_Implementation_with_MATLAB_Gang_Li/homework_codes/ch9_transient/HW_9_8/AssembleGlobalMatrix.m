function [K]= AssembleGlobalMatrix(K,ke,node_id_map,ndDOF)

n_nodes_per_element = size(node_id_map,1);

for i = 1:n_nodes_per_element    
  row_node = node_id_map(i,1);
  row=ndDOF*row_node - (ndDOF-1);
  for j = 1:n_nodes_per_element  
    col_node = node_id_map(j,1);
    col=ndDOF*col_node - (ndDOF-1);
    K(row:row+ndDOF-1, col:col+ndDOF-1)= ...
      K(row:row+ndDOF-1, col:col+ndDOF-1) + ...
      ke((i-1)*ndDOF+1:i*ndDOF,(j-1)*ndDOF+1:j*ndDOF);
  end
end
