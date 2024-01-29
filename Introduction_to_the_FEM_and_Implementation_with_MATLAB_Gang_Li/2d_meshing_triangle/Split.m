function Split(eid, pt)
global nodes edges last_node_row last_edge_row;
global sw_queue sw_head sw_tail;
global tris;
o=edges(eid,1);            % get the origin node of edge "eid"
d=edges(eid,2);            % get the destination node of edge "eid"
[eids,nids]=NeighborEdgesNodes(eid);  % get neighbor edges and nodes
DeleteTriByEdge(eid);      % delete triangles associated with eid
DeleteEdge(eid);           % delete the edge

if size(pt,2)==1           % pt is an existing node
  x=(nodes(o,1)+nodes(d,1))/2.0;
  y=(nodes(o,2)+nodes(d,2))/2.0;
else                       % pt is a newly created node
  x=pt(1); y=pt(2);
end
AddNode(x,y);

for i=1:size(nids,1)
  Connect(last_node_row,nids(i));
  if size(pt,2)==1 && (nids(i)==o || nids(i)==d)
    edges(last_edge_row,3)=2;     % flag edge as a boundary edge
  end
end
% if-block: create new triangles
if size(nids,1)==3
    if edges(eids(1),1)==nids(1) || edges(eids(1),2)==nids(1)
      tnids=[nids(1) nids(2) last_node_row ]';
      teids=[eids(1) last_edge_row-1 last_edge_row-2]';
      AddTriangle(tnids,teids);
      tnids=[last_node_row nids(2) nids(3)]';
      teids=[last_edge_row-1 eids(2) last_edge_row]';
      AddTriangle(tnids,teids);   
    else
      tnids=[nids(1) last_node_row nids(3)]';
      teids=[last_edge_row-2 last_edge_row eids(2)]';
      AddTriangle(tnids,teids);
      tnids=[last_node_row nids(2) nids(3)]';
      teids=[last_edge_row-1 eids(1) last_edge_row]';
      AddTriangle(tnids,teids);
    end
else
    tnids=[nids(1) nids(2) last_node_row];
    teids=[eids(1) last_edge_row-2 last_edge_row-3];
    AddTriangle(tnids,teids);
    tnids=[nids(2) nids(3) last_node_row];
    teids=[eids(2) last_edge_row-1 last_edge_row-2];
    AddTriangle(tnids,teids);
    tnids=[nids(3) nids(4) last_node_row];
    teids=[eids(3) last_edge_row last_edge_row-1];
    AddTriangle(tnids,teids);
    tnids=[nids(4) nids(1) last_node_row];
    teids=[eids(4) last_edge_row-3 last_edge_row];
    AddTriangle(tnids,teids);
end

% queue all the edges opposite to the new node
for i=1:size(eids,1)
  [sw_queue, sw_head, sw_tail]= ...
       QueueAdd(sw_queue, sw_head, sw_tail, eids(i));
end
LocalSwap();  % swap the edges if necessary