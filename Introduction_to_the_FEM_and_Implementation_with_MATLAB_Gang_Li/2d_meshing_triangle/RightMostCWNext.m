function [edge_out, node_out]=RightMostCWNext(node_in)
global nodes ray_angles TOL;

n_rays=nodes(node_in,3);
if n_rays==0 
  node_out=node_in;
  edge_out=0;
  return;
end
edge_out=nodes(node_in,4);
node_out=GetRayEndNode(node_in,edge_out);
dtheta=1.5*pi- ray_angles(node_in,1);

for i=2:n_rays
  t=1.5*pi- ray_angles(node_in,i);
  if t<dtheta
    edge_out=nodes(node_in,3+i);
    node_out=GetRayEndNode(node_in,edge_out);
    dtheta=t;
  end
end