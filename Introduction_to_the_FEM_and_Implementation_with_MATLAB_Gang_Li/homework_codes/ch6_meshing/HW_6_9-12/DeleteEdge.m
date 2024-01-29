function DeleteEdge(eid)
global edges nodes ray_angles n_edges;
if edges(eid,3)==0, return, end; % return if it is already deleted
edges(eid,3)=0;                  % flag to show the edge is deleted
orig=edges(eid,1); dest=edges(eid,2);

% next 17 lines: update the records of neighbor edges 
[opre_edge, onext_edge, dprev_edge, dnext_edge]=NeighborEdges(eid);
if edges(opre_edge,1)==orig
  edges(opre_edge,5)=edges(eid,5);
else edges(opre_edge,7)=edges(eid,5);
end
if edges(onext_edge,1)==orig
  edges(onext_edge,4)=edges(eid,4);
else edges(onext_edge,6)=edges(eid,4);
end
if edges(dprev_edge,1)==dest
  edges(dprev_edge,5)=edges(eid,7);
else edges(dprev_edge,7)=edges(eid,7);
end
if edges(dnext_edge,1)==dest
  edges(dnext_edge,4)=edges(eid,6);
else edges(dnext_edge,6)=edges(eid,6);
end

% for-loop: delete the edge ray from node records
for i=1:2
  node=edges(eid,i);
  nodes(node,3)=nodes(node,3)-1;
  for j=4:size(nodes,2)
    if nodes(node,j)==eid
      nodes(node,j:end)=[nodes(node,j+1:end) 0];
      ray_angles(node,j-3:end)=[ray_angles(node,j-2:end) 0];
      break;
    end
  end
end
n_edges=n_edges-1;