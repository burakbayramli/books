function [eids,nids]=NeighborEdgesNodes(eid)
global edges nodes;
[oprev,onext,dprev,dnext]=NeighborEdges(eid); % get neighbor edges
o=edges(eid,1); d=edges(eid,2);  % o=origin node, d=destination node
nop=GetRayEndNode(o,oprev);      % get the "other" end node of the  
non=GetRayEndNode(o,onext);      % connected edges
ndp=GetRayEndNode(d,dprev);
ndn=GetRayEndNode(d,dnext);
% next block: get neighbor edge
if edges(eid,3)==2
  nids=zeros(3,1);               % if it is a boundary edge
  if nop==ndn && non~=ndp
    eids=[oprev dnext]';         % two neighbor edges
    nids=[o nop d]';             % three nodes
  elseif non==ndp && nop~=ndn
    eids=[dprev onext]';         % two neighbor edges
    nids=[d non o]';             % three nodes 
  elseif non==ndp && nop==ndn 
    if CCW(o, d, non)>0
      eids=[dprev onext]';       % two neighbor edges
      nids=[d non o]';           % three nodes 
    else
      eids=[oprev dnext]';       % two neighbor edges
      nids=[o nop d]';           % three nodes 
    end
  else
    error('All corner angles should be >= 60 degree');
  end
elseif edges(eid,3)==1           % if it is an interior edge
  nids=zeros(4,1);
  eids=[oprev dnext dprev onext]';% four neighbor edges
  nids=[o nop d non]';            % four nodes
end