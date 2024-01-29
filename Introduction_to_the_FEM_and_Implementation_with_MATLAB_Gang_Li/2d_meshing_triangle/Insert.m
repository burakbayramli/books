function Insert(pt)
global edges TOL last_node_row tris last_edge_row;
global sw_queue sw_head sw_tail;

[tid,a,b]=FindBdngTriByPt(pt);
if abs(a)<TOL
  if abs(b)<TOL || abs(b-1)<TOL; return; 
  else Split(tris(tid,6),pt); return;
  end
elseif abs(b)<TOL
  if abs(a-1)<TOL; return; 
  else Split(tris(tid,4),pt); return;
  end
elseif abs(a+b-1)<TOL
  Split(tris(tid,5),pt);
  return;
end

nids=tris(tid,1:3)';
eids=tris(tid,4:6)';
AddNode(pt(1), pt(2));
for i=1:size(nids,1)
  Connect(last_node_row,nids(i));
end

%delete the old triangle
for i=1:size(eids,1)
  for k=9:10
    if edges(eids(i),k)==tid;
       edges(eids(i),k)=0;
    end
  end
end
tris(tid,6)=0;

% create new triangles
tnids=[last_node_row nids(1) nids(2)];
teids=[last_edge_row-2 eids(1) last_edge_row-1];
AddTriangle(tnids,teids);

tnids=[last_node_row nids(2) nids(3)];
teids=[last_edge_row-1 eids(2) last_edge_row];
AddTriangle(tnids,teids);

tnids=[last_node_row nids(3) nids(1)];
teids=[last_edge_row eids(3) last_edge_row-2];
AddTriangle(tnids,teids);

% enqueue all the edges opposite to the new node
for i=1:size(eids,1)
  if edges(eids(i),3)~=2
    [sw_queue, sw_head, sw_tail]= ...
    QueueAdd(sw_queue, sw_head, sw_tail, eids(i));
  end
end
LocalSwap();