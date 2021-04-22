%Script for Example 13.1a, Method 2.
delta=sqrt(pi/1800); x(1)=0;  y(1)=0;
nodecount=1; ncirc=floor(1/delta); minrad=1/ncirc;
for i=1:ncirc
    rad=i*minrad;
    nnodes=floor(2*pi*rad/delta);
    anglegap=2*pi/nnodes;
    for k=1:nnodes
      x(nodecount+1)=rad*cos(k*anglegap);
      y(nodecount+1)=rad*sin(k*anglegap);
      nodecount = nodecount+1;
    end
end


tri=delaunay(x,y); trimesh(tri,x,y), axis('equal')
%Remove comment symbol below to see just only the node plot.
%plot(x,y,'bo'), axis('equal') 