function inside = vertex_in_cell(vertex, cmin, dh)

inside = point_in_range( vertex(1), [cmin(1), cmin(1)+dh] ) ...
    && point_in_range( vertex(2), [cmin(2), cmin(2)+dh] );