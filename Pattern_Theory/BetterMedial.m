% This code assumes a polygon is given by a vector z of complex numbers
plot([z z(1)],'-*'), hold on, axis equal
xmax = max(real(z)); xmin = min(real(z)); ymax = max(imag(z)); ymin = min(imag(z));
maxlen = max((xmax-xmin), (ymax-ymin))/20;

% Get Delaunay triangles and compute their circumcenters
tri = sort(delaunay(real(z),imag(z))')';
u = z(tri(:,1)); v = z(tri(:,2)); w = z(tri(:,3));
dot = (u-w).*conj(v-w);
m = (u+v+i*(u-v).*real(dot)./imag(dot))/2;
r = abs(u-m);

% Prune the exterior triangles. m is now the vertices of the medial axis
inside = imag(dot) < 0;
triin = tri(inside,:);
m = m(inside); r = r(inside);
nt = size(triin,1);

% Find edges of the medial axis
B = sparse([1:nt 1:nt 1:nt], triin(:), ones(1,3*nt));
[a,b,c] = find(B*B'>1);
ind = a>b; a = a(ind); b = b(ind);
plot([m(a); m(b)], '-')

% Subdivide long segments in the medial axis
numsub = ceil(abs(m(a)-m(b))/maxlen);
ind = numsub > 1; delta = 1./numsub(ind);
newm = m(a(ind))*(delta:delta:1-delta) + m(b(ind))*((1-delta):-delta:delta);
newr = abs(newm - z(dsearch(real(z), imag(z), tri, real(newm), imag(newm))));
m = [m newm]; r = [r newr];
nm = size(m,2);
plot(m,'*') 

