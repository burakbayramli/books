function PlotMesh2d(nodes, elements, show_nid, show_eid, fig_id)

n_vertices=size(elements,2)-1;
if n_vertices==6; n_vertices=3; 
elseif n_vertices==8; n_vertices=4; 
end

figure(fig_id);
clf;
hold on;
for i=1:size(elements,1);
  x=nodes(elements(i,2:n_vertices+1),2);
  y=nodes(elements(i,2:n_vertices+1),3);
  pgon=polyshape(x,y);
  pg=plot(pgon);
  pg.FaceColor='white'; 
  if show_eid==true
    [cx, cy]=centroid(pgon);
    str=strcat('(', num2str(i),')');
    text(cx,cy,str,'FontSize',11,'color','k');
  end
end;

if show_nid==true
  for i=1:size(nodes,1);
    text(nodes(i,2), nodes(i,3),num2str(i),...
         'FontSize',11,'BackgroundColor','white');
  end
end
hold off;