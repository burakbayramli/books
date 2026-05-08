function area = overlap_pgram_cell(p0, v1, v2, cmin, dh)
% p0: starting vertex of pgram.
% v1 and v2 emanate from p0 to define pgram.
% cmin: cell start (min coordinates).
% cmin+dh: cell max coordinates.

% pgram vertices.
p1 = [p0(1)+v1(1), p0(2)+v1(2)];
p2 = [p0(1)+v2(1), p0(2)+v2(2)];
p3 = [p1(1)+v2(1), p1(2)+v2(2)];
% pgram segments.
s1 = [p0(1), p0(2); p1(1), p1(2)];
s2 = [p0(1), p0(2); p2(1), p2(2)];
s3 = [p1(1), p1(2); p3(1), p3(2)];
s4 = [p2(1), p2(2); p3(1), p3(2)];
% intersection points with cell.
vs1 = cell_intersections( s1, cmin, dh );
vs2 = cell_intersections( s2, cmin, dh );
vs3 = cell_intersections( s3, cmin, dh );
vs4 = cell_intersections( s4, cmin, dh );
% pgram vertices inside the cell.
pp = zeros(4,3);
if vertex_in_cell(p0,cmin,dh)
    pp(1,:) = [ p0(1), p0(2), 1 ];
end
if vertex_in_cell(p1,cmin,dh)
    pp(2,:) = [ p1(1), p1(2), 1 ];
end
if vertex_in_cell(p2,cmin,dh)
    pp(3,:) = [ p2(1), p2(2), 1 ];
end
if vertex_in_cell(p3,cmin,dh)
    pp(4,:) = [ p3(1), p3(2), 1 ];
end
% cell vertices inside the pgram.
cc = zeros(4,3);
if vertex_in_pgram(cmin,p0,v1,v2)
    cc(1,:) = [ cmin(1), cmin(2), 1 ];
end
if vertex_in_pgram([cmin(1)+dh, cmin(2)],p0,v1,v2)
    cc(2,:) = [ cmin(1)+dh, cmin(2), 1 ];
end
if vertex_in_pgram([cmin(1), cmin(2)+dh],p0,v1,v2)
    cc(3,:) = [ cmin(1), cmin(2)+dh, 1 ];
end
if vertex_in_pgram(cmin+dh,p0,v1,v2)
    cc(4,:) = [ cmin(1)+dh, cmin(2)+dh, 1 ];
end
% Finally, lets put together all of the vertices!!!
intersections = [ vs1( vs1(:,3) == 1, : );
    vs2( vs2(:,3) == 1, : );
    vs3( vs3(:,3) == 1, : );
    vs4( vs4(:,3) == 1, : ) ];
v_in_cell = pp( pp(:,3) == 1, : );
v_in_pgram = cc( cc(:,3) == 1, : );
vertices = [ intersections(:,1:2); v_in_cell(:,1:2); v_in_pgram(:,1:2) ];
if ~isempty(vertices)
    vertices = reorder_polygon_points(vertices);
    area = polyarea( vertices(:,1), vertices(:,2) );
else
    area = 0;
end




