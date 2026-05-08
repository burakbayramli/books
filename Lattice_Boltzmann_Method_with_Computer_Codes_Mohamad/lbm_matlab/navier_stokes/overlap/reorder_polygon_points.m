function vertices = reorder_polygon_points(vertices)
% Takes a group of vertices and reorders them so that the resulting matrix
%   can be traversed sequentially to describe a single closed loop 
%   describing a 2D convex polygon.
% vertices: n x 2 matrix; column is coordinate [x,y] and row is the point.
%   Describes an n-sided polygon.
% vertices should be non-repeating (but will still return correct results).
% adds first vertex to end to complete ppolygon description.

[n, ~] = size(vertices);
centroid = sum(vertices,1) / n;
r = [vertices(:,1) - centroid(1), vertices(:,2) - centroid(2)];
angles = atan2(r(:,2), r(:,1));
vertices = sortrows([vertices, angles], 3);
vertices = vertices(:,1:2);
vertices = [vertices; vertices(1,:)];