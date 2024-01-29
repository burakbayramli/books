clear all;

load nodes.dat;
load elements.dat;

nElements=size(elements,1);
nNodes=size(nodes,1);
nElementEdges=size(elements,2)-1;
startIndex=nodes(1,1);



figure(1);
clf;
hold off;

plot(nodes(1,2),nodes(1,3),'.');

hold on;

for i=1:nElements
  x0=0;
  y0=0;
  for j=1:nElementEdges-1
    x1=nodes(elements(i,j+1),2);
    y1=nodes(elements(i,j+1),3);
    x2=nodes(elements(i,j+2),2);
    y2=nodes(elements(i,j+2),3);
    plot([x1 x2],[y1 y2],'k-');
    x0=x0+x1;
    y0=y0+y1;
  end 
  x1=nodes(elements(i,2),2);
  y1=nodes(elements(i,2),3);
  plot([x1 x2],[y1 y2],'k-');
  x0=x0+x2;
  y0=y0+y2;
  x0=x0/nElementEdges;
  y0=y0/nElementEdges;

  %text(x0,y0,num2str(i),'FontSize',15,'color','k');
end;


for i=1:nNodes
  x0=nodes(i,2);
  y0=nodes(i,3)-0.1;
  if x0<5
  text(x0,y0,num2str(i),'FontSize',15);
end
end

hold off

axis([0 4.8 -0.5 1.5]);
xlabel('x');
ylabel('y');
set(gca,'fontsize',16);
print -depsc beam_mesh.eps
