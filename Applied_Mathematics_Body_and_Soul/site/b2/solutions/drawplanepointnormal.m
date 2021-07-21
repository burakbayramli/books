function drawplanepointnormal(point, normal, color)

%initgraphics;
%cleargraphics;

if(norm(cross(normal, [1, 0, 0])) < 1e-6)
  dir1 = cross(normal, [0, 1, 0]);
else
  dir1 = cross(normal, [1, 0, 0]);
end

dir2 = cross(normal, dir1);

ndir1 = dir1 / norm(dir1);
ndir2 = dir2 / norm(dir2);

a1 = 2;
a2 = 2;

p1 = point - a1 * ndir1 - a2 * ndir2;
p2 = point + a1 * ndir1 - a2 * ndir2;
p3 = point + a1 * ndir1 + a2 * ndir2;
p4 = point - a1 * ndir1 + a2 * ndir2;

drawtriangle(p1, p2, p3, color);
drawtriangle(p1, p3, p4, color);

drawarrow(point, point + normal);
