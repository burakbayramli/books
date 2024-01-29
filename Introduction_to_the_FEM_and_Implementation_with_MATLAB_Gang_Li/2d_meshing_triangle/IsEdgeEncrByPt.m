function re=IsEdgeEncrByPt(eid,pt)
global edges nodes TOL;

o=edges(eid,1);
d=edges(eid,2);
x=(nodes(o,1)+nodes(d,1))/2.0;
y=(nodes(o,2)+nodes(d,2))/2.0;

dist=norm([pt(1)-x pt(2)-y]);
if dist-TOL>edges(eid,8)/2.0
  re=0;
else 
  re=1;
end