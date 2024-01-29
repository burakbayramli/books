function LocalSwap()
global edges last_node_row last_edge_row ;
global tris last_tri_row;
global sw_queue sw_head sw_tail;

while sw_head<=sw_tail  % continue if the swap queue is not empty
  [sw_queue, sw_head, sw_tail, sw_edge] ...
     = QueueRemove(sw_queue, sw_head, sw_tail);
  [egs,nds]=NeighborEdgesNodes(sw_edge);
  if edges(sw_edge,3)==2; continue; end;
  if InCircle(nds(1), nds(2), nds(3), nds(4))>0  % if in circle 
    for j=1:size(egs,1)     % for all the neighbor edges
      if edges(egs(j),1)~=last_node_row ... % if edge not
      && edges(egs(j),2)~=last_node_row ... % connected 
      && edges(egs(j),3)~=2                 % to the center
        [sw_queue, sw_head, sw_tail]= ...   % add to sw_queue
        QueueAdd(sw_queue, sw_head, sw_tail, egs(j)); 
      end
    end % end for
    DeleteTriByEdge(sw_edge);      % delete the old tringles 
    Swap(sw_edge);                 % swap
    tnids=[nds(1) nds(2) nds(4)];  % create a new triangle
    teids=[egs(1) last_edge_row egs(4)];
    AddTriangle(tnids,teids);      % add the new triangle
    tnids=[nds(2) nds(3) nds(4)];  % create a new triangle
    teids=[egs(2) egs(3) last_edge_row];
    AddTriangle(tnids,teids);      % add the new triangle
  end % end if
end