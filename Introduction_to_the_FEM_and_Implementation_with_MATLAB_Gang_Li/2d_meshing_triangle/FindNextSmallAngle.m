function FindNextSmallAngle()
global nodes ray_angles last_node_row;
global sm_queue sm_head sm_tail;
global test_node no_bad_node angle_test_done; 
global MAX_LENGTH;

if test_node>last_node_row
  if no_bad_node==1
    angle_test_done=1;
    return;
  else
    test_node=1;
    no_bad_node=1;
  end
end

for j=1:nodes(test_node, 3)
  e=j+1;
  if j~=nodes(test_node, 3)
    dtheta=ray_angles(test_node, e)-ray_angles(test_node, j);
  else
    e=1;
    dtheta=ray_angles(test_node, e)+2*pi-ray_angles(test_node, j);
  end
  if dtheta<0.3613671239
    a=GetRayEndNode(test_node, nodes(test_node, 3+j));
    b=GetRayEndNode(test_node, nodes(test_node, 3+e));
    if norm(nodes(test_node,1:2)-nodes(a,1:2))<MAX_LENGTH/1000
      error('Convergence failure');
    end
    nids=[test_node a b];
    [sm_queue, sm_head, sm_tail]= ...
     QueueAdd(sm_queue, sm_head, sm_tail, nids);
     no_bad_node=0;
     break;
  end
end
test_node=test_node+1;