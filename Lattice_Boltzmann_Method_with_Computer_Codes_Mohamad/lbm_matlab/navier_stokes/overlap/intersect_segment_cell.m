function intersect = intersect_segment_cell(p0,ray,cmin,dh)
% returns 1 if the ray intersects the cell described by cmin and dh.
% p0: start point of segment.

% cell vertices.
c0 = cmin - p0;
c1 = [c0(1)+dh, c0(2)];
c2 = c0 + dh;
c3 = [c0(1), c0(2)+dh];
% test if all vertices are on one side.
pos0 = sign(cross2d(ray,c0));
pos1 = sign(cross2d(ray,c1));
pos2 = sign(cross2d(ray,c2));
pos3 = sign(cross2d(ray,c3));
% sum to see if all same.
possum = pos0 + pos1 + pos2 + pos3;
intersected = possum == 4 || possum == -4;
if intersected
    intersect = 0;
else
    intersect = 1;
end
% check for the case of the intersection being totally outside fluid
%   domain.
if intersected
    segment = [p0; p0+ray];
    points = cell_intersections(segment,cmin,dh);
    points = points(points(:,3)==1,1:2);
    if size(points,1) > 1
        xout = points(1,1) < 0 && points(2,1) < 0;
        yout = points(1,2) < 0 && points(2,2) < 0;
        if xout || yout
            intersect = 0;
        end
    end
end

