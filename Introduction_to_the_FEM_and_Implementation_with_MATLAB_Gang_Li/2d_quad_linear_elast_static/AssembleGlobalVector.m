% Assemble element vector fe into the global vector F
% Input: F, fe: global and local vectors, respectively
%        node_id_map: column vector of global node IDs corresponding
%        to the local nodes
% Input: ndDOF: nodal degrees of freedom 
% Output: assembled global vector F
function [F]= AssembleGlobalVector(F,fe,node_id_map,ndDOF)
n_nodes_per_element= size(node_id_map,1); % number of nodes in element
% for-loop block: assembly process
for i = 1:n_nodes_per_element       % loop over the nodes
  row_node = node_id_map(i,1);      % global ID for row node
  row=ndDOF*row_node - (ndDOF-1);   % row number in the global F
  F(row:row+ndDOF-1,1) = F(row:row+ndDOF-1,1) + ... % add fe to F
                        fe((i-1)*ndDOF+1:i*ndDOF,1); 
end