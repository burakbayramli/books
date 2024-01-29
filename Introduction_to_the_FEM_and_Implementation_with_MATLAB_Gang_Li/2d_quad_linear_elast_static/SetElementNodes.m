% Set up (1) the physical coordinates of an element and (2) global
% node IDs of the nodes of the element
% Input: ele: element ID
% Input: nodes, elements: input nodes and elements matrices 
%        (i.e. nodes.dat and elements.dat)
% Output: element_nodes: the physical coordinates of a 2-D element
%         in the format of [x1, y1; x2 y2; x3, y3; ...]
% Output: node_id_map: a column vector storing the global node IDs of
%         local nodes 1, 2, 3, ...
function [element_nodes, node_id_map]= SetElementNodes(ele, nodes, elements)
n_nodes_per_element=size(elements,2)-1;  % number of nodes in element
dimension=size(nodes,2)-1;               % dimension of the element
element_nodes=zeros(n_nodes_per_element,dimension); % result matrix
node_id_map=zeros(n_nodes_per_element,1);           % result vector 
% for-loop block: set up element_nodes and node_id_map
for i=1:n_nodes_per_element
    global_node_id=elements(ele,i+1);
    for j=1:dimension
        element_nodes(i,j) = nodes(global_node_id,j+1);
    end
    node_id_map(i,1) = global_node_id;
end