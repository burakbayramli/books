function re=BdryEdgeIntNd(eid)
global edges;
if edges(eid,3)~=2; fprintf("BdryEdgeIntNd error\n"); end;
[oprev,onext,dprev,dnext]=NeighborEdges(eid); % get neighbor edges
o=edges(eid,1);                  % origin node
d=edges(eid,2);                  % destination node
nop=GetRayEndNode(o,oprev);      % get the end nodes
non=GetRayEndNode(o,onext);      % of the neighbor edges
ndp=GetRayEndNode(d,dprev);      
ndn=GetRayEndNode(d,dnext);

% if-block: identify the correct interior node
if nop==ndn && non~=ndp
  re=nop;
elseif non==ndp && nop~=ndn
  re=non;
elseif non==ndp && nop==ndn
  if CCW(o, d, non)>0
    re=non;
  else
    re=nop;
  end
else
  error('All corner angles should be >= 60 degree');
end