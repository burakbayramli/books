function Connect(a,b)
global edges nodes ray_angles;
global n_nodes n_edges n_max_edges last_edge_row;
% next 7 lines: check if "edge" matrix is full, double the size if Yes
row=last_edge_row+1;
if row>n_max_edges 
  n_max_edges=n_max_edges*2;
  tmp=edges;
  edges=zeros(n_max_edges,10);
  edges(1:n_max_edges/2,:)=tmp;
end;

% next 6lines: add a new edge row
edges(row,1)=a;
edges(row,2)=b;  
edges(row,3)=1;   % flag: 0: deleted edge, 1: interior edge
edges(row,8)=sqrt((nodes(b,1)-nodes(a,1))^2+(nodes(b,2)-nodes(a,2))^2);
n_edges=n_edges+1;
last_edge_row=last_edge_row+1;

% next 3 lines: insert a ray to each of a, b
[edge_angle_a, edge_angle_b]=TwoNodeRayAngles(a, b);
InsertRay(a,1,row,edge_angle_a);
InsertRay(b,2,row,edge_angle_b);