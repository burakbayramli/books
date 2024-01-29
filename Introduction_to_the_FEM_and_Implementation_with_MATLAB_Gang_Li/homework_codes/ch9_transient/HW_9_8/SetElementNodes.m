%---------------------
function [element_nodes, node_id_map]= SetElementNodes(ele, nodes, elements)

n_nodes_per_element=size(elements,2)-1;
dimension=size(nodes,2)-1;
element_nodes=zeros(n_nodes_per_element,dimension);
node_id_map=zeros(n_nodes_per_element,1);

for i=1:n_nodes_per_element
    global_node_id=elements(ele,i+1);
    for j=1:dimension
        element_nodes(i,j) = nodes(global_node_id,j+1);
    end
    node_id_map(i,1) = global_node_id;
end

