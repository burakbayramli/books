function SetNodeCorner(nid, eid)
global nodes nd_corners;

for i=1:nodes(nid,3)
  if nodes(nid,3+i)==eid
    nd_corners(nid,3+i)=-1;
  end
end