function drawplanepointdirections(point, dir1, dir2, color)

%initgraphics;
%cleargraphics;

ndir1 = dir1 / norm(dir1);
ndir2 = dir2 / norm(dir2);

a1 = 10;
a2 = 10;

p1 = point - a1 * ndir1 - a2 * ndir2;
p2 = point + a1 * ndir1 - a2 * ndir2;
p3 = point + a1 * ndir1 + a2 * ndir2;
p4 = point - a1 * ndir1 + a2 * ndir2;

drawtriangle(p1, p2, p3, color);
drawtriangle(p1, p3, p4, color);

drawarrow(point, point + dir1);
drawarrow(point, point + dir2);
