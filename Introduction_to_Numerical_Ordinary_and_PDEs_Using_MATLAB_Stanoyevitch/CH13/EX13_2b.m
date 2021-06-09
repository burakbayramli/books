%Script for Example 13.2b.
xb(1)=0; yb(1)=0; rnodes=1999; %remaining nodes
newnodes=8; %nodes to be added on next circle
radcount=1; %counter for circles
oldnodes=1; %number of nodes already deployed
while newnodes < rnodes/2
  rad = 1 - 2^(-radcount);
  for i=1:newnodes
    xb(oldnodes+i)=rad*cos(2*pi*i/newnodes);
    yb(oldnodes+i)=rad*sin(2*pi*i/newnodes); 
  end
  oldnodes=oldnodes + newnodes; %update oldnodes
  rnodes = rnodes - newnodes; %update rnodes
  radcount=radcount+1; %update radcount
  newnodes = 2*newnodes; %update newnodes
end
% now deploy remaining nodes on boundary
for i=1:rnodes
    xb(oldnodes+i)=cos(2*pi*i/rnodes);
    yb(oldnodes+i)=sin(2*pi*i/rnodes);
end



tri=delaunay(xb,yb); trimesh(tri,xb,yb), axis('equal')
%Remove comment symbol below to see just only the node plot.
%plot(xb,yb,'bo'), axis('equal') 