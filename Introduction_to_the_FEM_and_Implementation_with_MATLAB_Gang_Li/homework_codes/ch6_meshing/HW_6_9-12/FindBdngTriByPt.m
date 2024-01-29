function [tid, a, b]=FindBdngTriByPt(pt)
global tris nodes last_tri_row;
re=-1;
tid=0;
for i=1:last_tri_row
  if tris(i,6)==0; continue; end;
  p1=nodes(tris(i,1),1:2)';
  p2=nodes(tris(i,2),1:2)';
  p3=nodes(tris(i,3),1:2)';
  [re, a, b]=PtInTriangle(p1, p2, p3, pt);
  if re==1 || re==0 
    tid=i;
    return;
  end
end
if re<0;  fprintf("findBdngTriByPt error\n"); end;