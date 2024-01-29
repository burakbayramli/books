% Return True if d lies inside the circle defined by a, b, and c
function re=InCircle(a,b,c,d)
global nodes;
xa=nodes(a,1); ya=nodes(a,2); 
xb=nodes(b,1); yb=nodes(b,2); 
xc=nodes(c,1); yc=nodes(c,2); 
xd=nodes(d,1); yd=nodes(d,2); 
A=[xa ya xa^2+ya^2 1
   xb yb xb^2+yb^2 1
   xc yc xc^2+yc^2 1
   xd yd xd^2+yd^2 1];
if det(A)>0; re=1; 
else; re=0;  
end;