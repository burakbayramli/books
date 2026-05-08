function d = compute_wall_distances(nodes)
% for the lid driven cavity problem.

x = linspace(0,1,nodes);
left_wall_distance = repmat(x,nodes,1);
right_wall_distance = repmat(fliplr(x),nodes,1);
minx = min(left_wall_distance, right_wall_distance);
bottom_wall_distance = repmat(flipud(x'),1,nodes);
top_wall_distance = repmat(x',1,nodes);
miny = min(bottom_wall_distance,top_wall_distance);
d = min(minx,miny);
