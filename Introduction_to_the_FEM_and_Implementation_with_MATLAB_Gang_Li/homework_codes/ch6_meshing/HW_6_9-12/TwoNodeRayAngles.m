function [angle_a, angle_b]=TwoNodeRayAngles(node_a, node_b)
global nodes;
angle_a= atan2(nodes(node_b,2)-nodes(node_a,2), ...     % atan(dy/dx)
                    nodes(node_b,1)-nodes(node_a,1));
if angle_a<0
  angle_a= angle_a+2*pi;   % make sure angle is 0-2*pi
end
angle_b= mod(angle_a +pi, 2*pi);   % the other angle is pi apart