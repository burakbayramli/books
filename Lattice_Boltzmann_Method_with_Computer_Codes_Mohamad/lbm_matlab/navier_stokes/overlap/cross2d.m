function area = cross2d(v1,v2)

if size(v1,2) > 1
    v1 = v1';
    v2 = v2';
end
v3 = cross([v1;0],[v2;0]);
area = v3(3);