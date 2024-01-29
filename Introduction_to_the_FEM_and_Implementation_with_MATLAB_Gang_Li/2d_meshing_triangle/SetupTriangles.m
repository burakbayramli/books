function SetupTriangles()
global edges nodes ray_angles;
global n_nodes n_edges n_max_edges last_edge_row;
global tris n_tris n_max_tris last_tri_row nd_corners;
% next 5 lines: set up empty triangular matrix and initialization
nd_corners=nodes;
n_tris=0;
n_max_tris=10;
tris=zeros(n_max_tris,6);      
last_tri_row=0;       

% set up the triangles based on the results of initial triangulation  
for i=1:n_nodes                   % loop over the nodes
  n_corners=nd_corners(i,3);      % get the number of connectd edges      
  p1=i;                           
  for j=1:n_corners               % loop over the edges connected 
    if nd_corners(i,j+3) ~=-1     % to the center node
      p2=GetRayEndNode(i,nodes(i,j+3));
      next_edge_id=nodes(i, 4+mod(j,n_corners));
      p3=GetRayEndNode(i,next_edge_id);
      [sedge, tp]=GetNbrRayEndNode(p2, i, -1);
      [eedge, tp]=GetNbrRayEndNode(p3, i, 1); 
      if (sedge==eedge) && (CCW(p1,p2,p3)>0)
        row=last_tri_row+1;
        if row>n_max_tris 
          n_max_tris=n_max_tris*2;
          tmp=tris;
          tris=zeros(n_max_tris,6);
          tris(1:n_max_tris/2,:)=tmp;
        end;
        tris(row,1:3)=[p1 p2 p3];
        tris(row,4:6)=[nodes(i,j+3),eedge,next_edge_id];
        nd_corners(i,j+3)=-1;
        SetNodeCorner(p2,eedge);
        SetNodeCorner(p3,next_edge_id);
        last_tri_row=row;
        n_tris=n_tris+1;
      end
    end
  end
end

% for-loop: update the relevant entries in "edge" matrix
for i=1:last_tri_row
  for j=4:6
    e_id=tris(i,j);
    if edges(e_id,9)==0;  edges(e_id,9)=i;
    elseif edges(e_id,10)==0;  edges(e_id,10)=i;
    else fprintp('Setup tris error\n');
    end
  end
end