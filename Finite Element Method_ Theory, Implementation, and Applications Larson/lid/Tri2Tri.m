function neighbors = Tri2Tri(p,t)
np=size(p,2); % number of vertices
nt=size(t,2); % number of triangles
n2e=sparse(np,nt); % node-to-element adjacency matrix
% n2e(i,j)=1 means node "i" is in element "j"
for i=1:nt
n2e(t(1:3,i),i)=ones(3,1);
end
neighbors=-ones(nt,3); % -1 means no neighbor
for i=1:nt
% 1st edge lies between nodes t(2,i) and t(3,i), so search
% the adjacency matrix for elements sharing these two nodes
nb=intersect(find(n2e(t(2,i),:)),find(n2e(t(3,i),:)));
nb=setdiff(nb,i); % remove element "i" from neighbors "nb"
if isscalar(nb), neighbors(i,1)=nb(1); end
% 2nd edge
nb=intersect(find(n2e(t(3,i),:)),find(n2e(t(1,i),:)));
nb=setdiff(nb,i);
if isscalar(nb), neighbors(i,2)=nb(1); end
% 3rd edge
nb=intersect(find(n2e(t(1,i),:)),find(n2e(t(2,i),:)));
nb=setdiff(nb,i);
if isscalar(nb), neighbors(i,3)=nb(1); end
end
