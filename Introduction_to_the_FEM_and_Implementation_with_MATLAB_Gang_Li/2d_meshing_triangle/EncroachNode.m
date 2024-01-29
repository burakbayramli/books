function nid=EncroachNode(eid)
global nodes edges TOL;
nid=0;
if edges(eid,3)~=2  return; end;
nid=BdryEdgeIntNd(eid);
if ~IsEdgeEncrByPt(eid,[nodes(nid,1) nodes(nid,2)])
  nid=0;
end