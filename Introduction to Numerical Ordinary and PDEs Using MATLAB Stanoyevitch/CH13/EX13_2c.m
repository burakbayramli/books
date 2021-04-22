%Script for Example 13.2c.
n=0; nodecount=1;
while n<16
  s=sqrt(3*pi/2)/10/2^n; hgrid=2/s/2^n; vgrid=4/s/2^n;
%these will be sufficient horizontal and vertical grid counts to %create a rectangular grid (with gap size =s) that will cover the %domain Omega_n 
  for i=1:hgrid
  for j=1:vgrid
   xnew=1-2/2^n+i*s; ynew=-2/2^n+j*s;
   pij = [xnew ynew]; p=[1 0];
 if norm(pij,2)<1-s/2 & norm(pij-p,2)<2/2^n & norm(pij-p,2)>1/2^n+s/2
%The three conditions here check to see if the node should be added.
%The first says that the node should be in the unit circle (with a %safe distance to the boundary to prevent interior nodes from getting %too close to boundary nodes which will be added later).  The second %and third state that the distance from the node to the special %boundary point (1,0) should be between the two required radii.  The %last condition has a safety term added to the lower bound to prevent %nodes from successive iterations from getting too close.  
   x(nodecount)=xnew; y(nodecount)=ynew; nodecount=nodecount+1;
  end
 end
 end
%The next part of the loop puts nodes on the boundary.
 theta1=acos(1-2/2^(2*n)); theta2=acos(1-2^(-2*n)/2);
 if n==0, theta1=theta1-s; end 
 for theta = theta1:-s:(theta2+s/2)
  x(nodecount)=cos(theta); y(nodecount)=sin(theta);
  x(nodecount+1)=cos(theta); y(nodecount+1)=-sin(theta);
  nodecount=nodecount+2;
 end
 n=n+1;
end
%We need to put a node at the special unsymmetric point (-1,0). 
x(nodecount)=-1; y(nodecount)=0; nodecount=nodecount+1;
%Finally we put nodes in the portion of the domain between (1,0) 
%and the last Omega_n, and then on the boundary.
%We need first to bump n back down one unit.
n=n-1;
for i=1:hgrid
for j=1:vgrid
  xnew=1-2/2^n+i*s; ynew=-2/2^n+j*s;
  pij = [xnew ynew]; p=[1 0];
  if norm(pij,2)<1-s/2 & norm(pij-p,2)< 1/2^n 
   x(nodecount)=xnew; y(nodecount)=ynew; nodecount=nodecount+1;
  end
end
end
for theta = -theta2:s:theta2
  x(nodecount)=cos(theta); y(nodecount)=sin(theta);
  nodecount=nodecount+1;
end



tri=delaunay(x,y); trimesh(tri,x,y), axis('equal')
%Remove comment symbol below to see just only the node plot.
%plot(x,y,'bo'), axis('equal') 