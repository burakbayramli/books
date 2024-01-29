function [eid, nid]=GetNbrRayEndNode(center_nd, ray_nd_in, flag)
global edges nodes ray_angles;
n_rays=nodes(center_nd,3);
for i=1:n_rays
  edge_id=nodes(center_nd,i+3);
	if (edges(edge_id,1)==ray_nd_in) || (edges(edge_id,2)==ray_nd_in)
    if flag==1                         % next ray
      nbr_edge_id=nodes(center_nd, 4+mod(i,n_rays));
    else                               % prev ray
      nbr_edge_id=nodes(center_nd, 4+mod(i-2+n_rays,n_rays));
    end
  	nid=GetRayEndNode(center_nd, nbr_edge_id);
    eid=nbr_edge_id;  
    return;
  end
end
nid=center_nd;
fprintf('failed ray operation\n');