%Script for Example 13.1a, Method 1.
delta =2/46; counter=1;
for i=1:45
 for j=1:45
  x0(counter)=-1+i*delta; y0(counter)=-1+j*delta;
  counter=counter+1;
 end
end
counter=1;
for i=1:2025
  if norm([x0(i) y0(i)],2) < 1-delta/2
     x(counter)=x0(i); y(counter)=y0(i);
     counter=counter+1;
  end
end
for i=1:145
    x(i+1597)=cos(2*pi/145*i); y(i+1597)=sin(2*pi/145*i);
end

tri=delaunay(x,y); trimesh(tri,x,y), axis('equal')
%Remove comment symbol below to see just only the node plot.
%plot(x,y,'bo'), axis('equal') 