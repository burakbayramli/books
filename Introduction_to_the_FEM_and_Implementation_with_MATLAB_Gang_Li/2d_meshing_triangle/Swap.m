function Swap(a)
global edges;
if edges(a,3)==0 || edges(a,3)==2, return, % skip if not an interior
end;                                       % edge
[oprev, onext, dprev, dnext]=NeighborEdges(a); % get neighbor edges
orig=edges(a,1);
dest=edges(a,2);

% if-block: assign correct node1
if edges(oprev,1)==orig;  node1=edges(oprev,2);
else  node1=edges(oprev,1);
end
% if-block: assign correct node2
if edges(onext,1)==orig;  node2=edges(onext,2);
else  node2=edges(onext,1);
end

% if-block: connect the new node1 and node2, delete the old edge
if node1~=node2
  DeleteEdge(a);
  Connect(node1, node2);
else fprintf("Swap error\n");
end