
load elements.dat;
load feU.dat;
nodes=feU(:,1:3);

nElements=size(elements,1);
nNodes=size(nodes,1);
nElementEdges=size(elements,2)-1;


figure(1);
clf;
hold off;
dnodes(:,2:3)=nodes(:,2:3)+feU(:,4:5)*100;

plot(dnodes(1,2),dnodes(1,3),'.');

hold on;

for i=1:nElements
  for j=1:nElementEdges-1
    x1=nodes(elements(i,j+1),2);
    y1=nodes(elements(i,j+1),3);
    x2=nodes(elements(i,j+2),2);
    y2=nodes(elements(i,j+2),3);
    plot([x1 x2],[y1 y2],'k:','Linewidth',2);
  end 
  x1=nodes(elements(i,2),2);
  y1=nodes(elements(i,2),3);
  plot([x1 x2],[y1 y2],'k:','Linewidth',2);
end;

for i=1:nElements
  x0=0;
  y0=0;
  for j=1:nElementEdges-1
    x1=dnodes(elements(i,j+1),2);
    y1=dnodes(elements(i,j+1),3);
    x2=dnodes(elements(i,j+2),2);
    y2=dnodes(elements(i,j+2),3);
    plot([x1 x2],[y1 y2],'k-','Linewidth',2);
    x0=x0+x1;
    y0=y0+y1;
  end 
  x1=dnodes(elements(i,2),2);
  y1=dnodes(elements(i,2),3);
  plot([x1 x2],[y1 y2],'k-','Linewidth',2);
  x0=x0+x2;
  y0=y0+y2;
  x0=x0/nElementEdges;
  y0=y0/nElementEdges;
  %text(x0-0.05,y0,"element "+num2str(i),'FontSize',15,'color','k');
end;


hold off

