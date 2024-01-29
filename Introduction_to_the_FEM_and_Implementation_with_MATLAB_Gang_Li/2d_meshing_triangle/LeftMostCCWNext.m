function [edge_out, node_out]=LeftMostCCWNext(node_in)
global nodes ray_angles TOL;

n_rays=nodes(node_in,3);
if n_rays==0 
  node_out=node_in;
  edge_out=0;
  return;
end
edge_out=nodes(node_in,4);
node_out=GetRayEndNode(node_in,edge_out);
dtheta=mod(ray_angles(node_in,1)+TOL-pi*1.5,pi*2);

for i=2:n_rays
  t=mod(ray_angles(node_in,i)+TOL-pi*1.5,pi*2);
  if t<dtheta
    edge_out=nodes(node_in,3+i);
    node_out=GetRayEndNode(node_in,edge_out);
    dtheta=t;
  end
end