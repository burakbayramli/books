function inside = vertex_in_pgram(v, p0, v1, v2)
% returns 1 if vertex v is in the pgram described by p0, v1, and v2.
% p0 is a corner of the pgram. v1 and v2 emanate from p0 to form half of
% the pgram.

r = [v(1) - p0(1); v(2) - p0(2)]; % relative position of v to p0.

d1 = norm(v1,2);
d2 = norm(v2,2);
u1 = v1 / d1;
u2 = v2 / d2;

ccw = [0, -1; 1, 0];
n1 = ccw * u1;
n2 = -ccw * u2;
if dot(n1,v2) < 0
    n1 = -n1;
    n2 = -n2;
end

inside = ...
    dot(n1,r) >= 0 && dot(n1,r) <= dot(n1,v2) ...
    && dot(n2,r) >= 0 && dot(n2,r) <= dot(n2,v1);