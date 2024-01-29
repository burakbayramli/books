function DelaunayRefinement()
global edges last_edge_row angle_test_done edge_test_done;
global ec_queue ec_head ec_tail;
global sm_queue sm_head sm_tail sm_nodes;
global sw_queue sw_head sw_tail;
global test_node test_edge no_bad_node no_bad_edge;
% next line: create queue of encroached edges
[ec_queue, ec_head, ec_tail] = QueueCreate(2); 
% next line: create queue of candidate swap edges
[sw_queue, sw_head, sw_tail] = QueueCreate(1);

while 1  % continue until exit from inside
  % for-loop: enqueue all the encroached edges
  for i=1:last_edge_row
    n_id=EncroachNode(i);
    if n_id>0
      [ec_queue, ec_head, ec_tail]= ...
       QueueAdd(ec_queue, ec_head, ec_tail, [i n_id]);
    end
  end
  if ec_head>ec_tail; break; end; % exit while-loop if queue is empty
  % while-loop: split all the encroached edges
  while ec_head<=ec_tail
    [ec_queue, ec_head, ec_tail, ec_edge] ...
       = QueueRemove(ec_queue, ec_head, ec_tail);
    Split(ec_edge(1),ec_edge(2));
  end
end

% next line: create queue of small angle and large size triangles 
[sm_queue, sm_head, sm_tail] = QueueCreate(3);
test_node=1;   test_edge=1;        % set up flags
no_bad_node=1; no_bad_edge=1;      % set up flags

while 1 % continue until exit from inside
  % if-block: find next small angle or large triangle, add to sm_queue
  if angle_test_done==0
    FindNextSmallAngle();
  elseif edge_test_done==0
    FindNextLargeEdge();
  else
    break;
  end
  while sm_head<=sm_tail   % continue until sm_queue is empty
    [sm_queue, sm_head, sm_tail, sm_nodes] ...
    = QueueRemove(sm_queue, sm_head, sm_tail); 
    % get the next triangle in the sm_queue
    [x,y,r]=CompCircumcircle(sm_nodes(1),sm_nodes(2), sm_nodes(3)); 
    flag=0;
    % for-loop: insert a node at its circumcenter
    for i=1:last_edge_row
      if edges(i,3)==2 && IsEdgeEncrByPt(i,[x y])
        flag=1;
        nid=BdryEdgeIntNd(i);
        Split(i,nid);
        break;
      end
    end
    if flag==0;  Insert([x y]); end;
  end
end