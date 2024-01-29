function DeleteOutsideEdges()
global edges nodes last_edge_row;
for i=1:last_edge_row               % loop over the edges
  if edges(i,3)~=1; continue; end;  % if it's boundary edge, continue
  if ~IsInShape(i)                  % if the edge is inside the shape
    DeleteEdge(i);                  % delete it
  end
end