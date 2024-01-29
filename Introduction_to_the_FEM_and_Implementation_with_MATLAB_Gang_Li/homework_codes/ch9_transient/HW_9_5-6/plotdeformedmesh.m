clear all;
load nodes.dat;
load elements.dat;
load feU.dat;

nElements=size(elements,1);
nNodes=size(nodes,1);
nElementEdges=size(elements,2)-1;
startIndex=nodes(1,1);

if startIndex==0  %for nodes.dat generated from C++ code
  idx=1;
else              %for nodes.dat generated from matlab code
  idx=0;
end


figure(1);
clf;
hold off;


plot(nodes(1,2),nodes(1,3),'.');

hold on;

for i=1:nElements
  x0=0;
  y0=0;
  for j=1:nElementEdges-1
    x1=nodes(elements(i,j+1)+idx,2);
    y1=nodes(elements(i,j+1)+idx,3);
    x2=nodes(elements(i,j+2)+idx,2);
    y2=nodes(elements(i,j+2)+idx,3);
    plot([x1 x2],[y1 y2],'k:');
    x0=x0+x1;
    y0=y0+y1;
  end 
  x1=nodes(elements(i,2)+idx,2);
  y1=nodes(elements(i,2)+idx,3);
  plot([x1 x2],[y1 y2],'k:');
  %x0=x0+x2;
  %y0=y0+y2;
  %x0=x0/nElementEdges;
  %y0=y0/nElementEdges;
  %text(x0,y0,num2str(i),'FontSize',5,'color','r');
end;



nodes(:,2:3)=nodes(:,2:3)+feU(:,4:5)*6;

for i=1:nElements
  x0=0;
  y0=0;
  for j=1:nElementEdges-1
    x1=nodes(elements(i,j+1)+idx,2);
    y1=nodes(elements(i,j+1)+idx,3);
    x2=nodes(elements(i,j+2)+idx,2);
    y2=nodes(elements(i,j+2)+idx,3);
    plot([x1 x2],[y1 y2],'k-');
    x0=x0+x1;
    y0=y0+y1;
  end 
  x1=nodes(elements(i,2)+idx,2);
  y1=nodes(elements(i,2)+idx,3);
  plot([x1 x2],[y1 y2],'k-');
  %x0=x0+x2;
  %y0=y0+y2;
  %x0=x0/nElementEdges;
  %y0=y0/nElementEdges;
  %text(x0,y0,num2str(i),'FontSize',5,'color','r');
end;


%for i=1:nNodes
%  x0=nodes(i,2);
%  y0=nodes(i,3);
%  text(x0,y0,num2str(i),'FontSize',5);
%end

set(gca,'fontsize',16);
axis([0 21 -1.5 2.5]);
xlabel('x');
ylabel('y');

hold off
print -depsc mode4.eps
