function AddNode(x,y)
global nodes ray_angles last_node_row;
row=last_node_row+1;   % current row pointer = the last non-empty 
                       % row of the "nodes" matrix + 1
% if block: if the "nodes" matrix if full, double the size
if row>size(nodes,1)        
  tmp=nodes;
  nodes=zeros(last_node_row*2,size(tmp,2));
  nodes(1:last_node_row,:)=tmp;
  tmp=ray_angles;
  ray_angles=zeros(last_node_row*2,size(tmp,2));
  ray_angles(1:last_node_row,:)=tmp;
end;
nodes(row,1:2)=[x y];   % add the node at the row pointed by "row"
last_node_row=last_node_row+1;   % the last non-empty row + 1