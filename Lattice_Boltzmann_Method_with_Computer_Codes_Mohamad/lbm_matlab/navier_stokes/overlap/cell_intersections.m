function points = cell_intersections(segment,cmin,dh)
% Computes the intersection points between segment and the square Cartesian
% 2D cell whose lower left corner is at cmin and upper right corner is at
% [cmin(1)+dh,cmin(2)+dh].
% segment: row 1 is the start point and row 2 is the end point.
% points: a point of intersection for each side. Each row is for a side,
% column 1 gives x-coordinate, column 2 gives y-coordinate, and column 3 is
% a boolean flag indicating whether an intersection exists or not.
% points(1): bottom
% points(2): right
% points(3): top
% points(4): left

segment = sortrows(segment,1); % lowest x first, (in tie lowest y first).

points = zeros(4,3);
cxr = [cmin(1), cmin(1) + dh]; % cell x range.
cyr = [cmin(2), cmin(2) + dh]; % cell y range.
sxr = segment(:,1); % segment x range.
syr = sortrows(segment(:,2),1); % segment y range.
sdx = diff(segment(:,1)); % segment dx.
sdy = diff(segment(:,2)); % segment dy.

if sdx == 0
    sx = sxr(1);
    if ( point_in_range( sx, cxr ) )
        if point_in_range( cyr(1), syr )
            points(1,:) = [ sx, cyr(1), 1 ];
        end
        if point_in_range( cyr(2), syr )
            points(3,:) = [ sx, cyr(2), 1 ];
        end
    end
else
    m = sdy/sdx; % segment slope.
    x0 = segment(1,1);
    y0 = segment(1,2);
    % check left side intersection.
    if point_in_range( cxr(1), sxr )
        x = cxr(1) - x0;
        y = m*x + y0;
        if point_in_range( y, cyr )
            points(4,:) = [ cxr(1), y, 1 ];
        end
    end
    % check right side intersection.
    if point_in_range( cxr(2), sxr )
        x = cxr(2) - x0;
        y = m*x + y0;
        if point_in_range( y, cyr )
            points(2,:) = [ cxr(2), y, 1 ];
        end
    end
    % check bottom side intersection.
    if point_in_range( cyr(1), syr )
        y = cyr(1) - y0;
        x = 1/m*y + x0;
        if point_in_range( x, cxr )
            points(1,:) = [ x, cyr(1), 1 ];
        end
    end
    % check top side intersection.
    if point_in_range( cyr(2), syr )
        y = cyr(2) - y0;
        x = 1/m*y + x0;
        if point_in_range( x, cxr )
            points(3,:) = [ x, cyr(2), 1 ];
        end
    end
end

