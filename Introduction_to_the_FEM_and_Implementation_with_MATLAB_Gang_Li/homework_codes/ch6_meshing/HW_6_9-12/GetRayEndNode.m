function re=GetRayEndNode(nd,edge_id)
global edges;
% if block: get the end node of the edge that is away from node "nd"
if edges(edge_id,1)==nd;  re=edges(edge_id,2);
else re=edges(edge_id,1);
end