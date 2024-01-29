% Assemble element matrix ke into the global matrix K
% Input: K, ke: global and local matrices, respectively
%        node_id_map: column vector of global node IDs corresponding
%        to the local nodes
% Input: ndDOF: nodal degrees of freedom 
% Output: assembled global matrix K
function [K]= AssembleGlobalMatrix(K,ke,node_id_map,ndDOF)
n_nodes_per_element= size(node_id_map,1); % number of nodes in element
% for-loop block: assembly process
for i = 1:n_nodes_per_element       % loop over the nodes
  row_node = node_id_map(i,1);      % global ID for row node
  row=ndDOF*row_node - (ndDOF-1);   % row number in the global K
  for j = 1:n_nodes_per_element     % loop over the nodes
    col_node = node_id_map(j,1);    % global ID for col node
    col=ndDOF*col_node - (ndDOF-1); % column number in the global K
    K(row:row+ndDOF-1, col:col+ndDOF-1)= ...       % add entry or
      K(row:row+ndDOF-1, col:col+ndDOF-1) + ...    % block of ke onto
      ke((i-1)*ndDOF+1:i*ndDOF,(j-1)*ndDOF+1:j*ndDOF); % K
  end
end