function [le,re]=BuildDelaunay(sid,eid)
global nodes last_edge_row dflag;
n_node_ids=eid-sid+1;
p1=sid; p2=sid+1;
if n_node_ids==2             % if number of nodes==2
  Connect(p1,p2);            % connect the two nodes to form an edge
  le=last_edge_row; re=le;   
elseif n_node_ids==3         % if number of nodes==3
  p3=eid;                    
  Connect(p1,p2); a=last_edge_row;  % connect three nodes to form 
  Connect(p2,p3); b=last_edge_row;  % a triangle
  Connect(p3,p1); c=last_edge_row;
  if CCW(p1,p2,p3)>0 
    le=a; re=b; 
  else 
    le=c; re=c;
  end
else
  [left_s,left_e, right_s,right_e]=DivideInX(sid,eid); % divide points
  [ldo,ldi]=BuildDelaunay(left_s, left_e);    % recursive call (left)
  [rdi,rdo]=BuildDelaunay(right_s, right_e);  % recursive call (right)
  le=ldo; re=rdo;
  [Lp,Rp]=CompLCT(left_s,left_e, right_s,right_e); % get lowest 
                                                   % common tangent
  Merge(Lp,Rp, left_s,left_e, right_s,right_e);    % merge two halves
  if dflag==1, return, end;
end