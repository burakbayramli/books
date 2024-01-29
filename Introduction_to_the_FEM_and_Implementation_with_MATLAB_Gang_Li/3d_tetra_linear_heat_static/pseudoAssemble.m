%---------
% assemble ke into global K
%---------
for i = 1:n_nodes_per_element    
    row = global_id_of_element_node(i);
    for j = 1:n_nodes_per_element  
        col = global_id_of_element_node(j);
        K(row, col)= K(row, col) + ke(i,j);
    end
end