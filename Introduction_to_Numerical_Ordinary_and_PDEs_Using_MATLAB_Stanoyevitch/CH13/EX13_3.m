%Script for Example 13.3.
delta=sqrt(3*pi/350);
nodecount=1; ncirc=floor(1/delta); radgap=1/ncirc;

for i=0:ncirc
  rad=1+i*radgap; nnodes=floor(2*pi*rad/delta); anglegap=2*pi/nnodes;
  for k=1:nnodes
  x(nodecount)=rad*cos(k*anglegap); y(nodecount)=rad*sin(k*anglegap);
   nodecount = nodecount+1;
  end
end
tri=delaunay(x,y); trimesh(tri,x,y), axis('equal')
size(x)

x(400)=0; y(400)=0;
tri=delaunay(x,y); trimesh(tri,x,y), axis('equal')

badelcount=1;
for ell=1:722
  if ismember(400,tri(ell,:))
   badel(badelcount)=ell;
   badelcount=badelcount+1;
  end
end

tri=tri(setdiff(1:722,badel),:);
x=x(1:399); y=y(1:399);
trimesh(tri,x,y), axis('equal')

