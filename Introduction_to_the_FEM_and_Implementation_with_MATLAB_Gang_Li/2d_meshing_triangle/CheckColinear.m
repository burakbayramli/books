function [lp_out, rp_out]=CheckColinear(Lp, Rp)
global edges nodes ray_angles;
global n_nodes n_edges n_max_edges last_edge_row;
[angle_a, angle_b]=TwoNodeRayAngles(Lp, Rp); % angles of line Lp-Rp
lp_out=Lp;
rp_out=Rp;
% next 6 lines: check if any left-side edge is collinear with Lp-Rp
n_rays=nodes(Lp,3);
for i=1:n_rays
  if abs(ray_angles(Lp,i)-angle_a)<1e-5
    lp_out=GetRayEndNode(Lp,nodes(Lp,3+i)); % get the collinear node
  end
end
% next 6 lines: check if any right-side edge is collinear with Lp-Rp
n_rays=nodes(Lp,3);
n_rays=nodes(Rp,3);
for i=1:n_rays
  if abs(ray_angles(Rp,i)-angle_b)<1e-5
    rp_out=GetRayEndNode(Rp,nodes(Rp,3+i)); % get the collinear node
  end
end