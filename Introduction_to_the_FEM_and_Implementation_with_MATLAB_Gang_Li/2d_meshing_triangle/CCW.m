% Return True iff p1, p2, p3 form a counterclockwise oriented triangle
function re=CCW(p1,p2,p3)
global nodes;
x1=nodes(p1,1); y1=nodes(p1,2);
x2=nodes(p2,1); y2=nodes(p2,2);
x3=nodes(p3,1); y3=nodes(p3,2);
a=(x1-x3) * (y2-y3) - (x2-x3) * (y1-y3);
if  a > 1e-5      
  re=1;          % true
elseif a<-1e-5
  re=-1;         % false
else 
  re=0;          % collinear case
end