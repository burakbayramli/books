function PlotEdges()
global edges nodes ray_angles last_edge_row;

hold on;
for i=1:last_edge_row
  if edges(i,3)>0
    orig=edges(i,1);
    dest=edges(i,2);
    plot([nodes(orig,1) nodes(dest,1)],...
     [nodes(orig,2) nodes(dest,2)],'-k','LineWidth',edges(i,3));
  end
end
hold off