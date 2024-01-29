function FindNextLargeEdge()
global edges last_edge_row sm_queue sm_head sm_tail;
global MAX_LENGTH test_edge no_bad_edge edge_test_done;

if test_edge>last_edge_row
  if no_bad_edge==1
    edge_test_done=1;
    return;
  else
    test_edge=1;
    no_bad_edge=1;
  end
end

if edges(test_edge,3)==0;
   test_edge=test_edge+1;
  return; 
end;

if edges(test_edge,8)>MAX_LENGTH
    [eids,nids]=NeighborEdgesNodes(test_edge);
    [sm_queue, sm_head, sm_tail]= ...
    QueueAdd(sm_queue, sm_head, sm_tail, nids(1:3,1));
    no_bad_edge=0;
end
test_edge=test_edge+1;