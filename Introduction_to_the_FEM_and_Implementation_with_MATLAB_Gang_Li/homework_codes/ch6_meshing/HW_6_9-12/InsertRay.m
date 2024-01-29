function InsertRay(nd,type,edge_row,edge_angle)
global edges nodes ray_angles;
global n_nodes n_edges n_max_edges last_edge_row;
% if-block: insert the ray in the node record
n_rays=nodes(nd,3);
if n_rays==0
  nodes(nd,3)=1;
  nodes(nd,4)=edge_row;
  ray_angles(nd,1)=edge_angle;
else  
  tmp(:,1)=[nodes(nd,4:3+n_rays) edge_row];
  tmp(:,2)=[ray_angles(nd,1:n_rays) edge_angle];
  tmp=sortrows(tmp,2);
  nodes(nd,4:3+n_rays+1)=tmp(:,1);
  ray_angles(nd,1:n_rays+1)=tmp(:,2);
  nodes(nd,3)=nodes(nd,3)+1;
end

% for-loop: set up the opre, onext, dpre, dnext entries
n_rays=nodes(nd,3);
for i=1:n_rays
  if edge_row==nodes(nd,3+i)
    if type==1    
      edges(edge_row,4)=nodes(nd, 4+mod(i-2+n_rays,n_rays));
      edges(edge_row,5)=nodes(nd, 4+mod(i,n_rays));
      orig=edges(edge_row,1);
      opre_edge=edges(edge_row,4);
      if edges(opre_edge,1)==orig
        edges(opre_edge,5)=edge_row;
      else
        edges(opre_edge,7)=edge_row;
      end
      onext_edge=edges(edge_row,5);
      if edges(onext_edge,1)==orig
        edges(onext_edge,4)=edge_row;
      else
        edges(onext_edge,6)=edge_row;
      end
    else
      edges(edge_row,6)=nodes(nd, 4+mod(i-2+n_rays,n_rays));
      edges(edge_row,7)=nodes(nd, 4+mod(i,n_rays));
      dest=edges(edge_row,2);
      dprev_edge=edges(edge_row,6);
      if edges(dprev_edge,1)==dest
        edges(dprev_edge,5)=edge_row;
      else
        edges(dprev_edge,7)=edge_row;
      end
      dnext_edge=edges(edge_row,7);
      if edges(dnext_edge,1)==dest
       edges(dnext_edge,4)=edge_row;
      else
        edges(dnext_edge,6)=edge_row;
      end
    end
  end
end